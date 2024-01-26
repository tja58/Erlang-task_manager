-module(project).

-export([start/1, rpc/2, run/1]).

%%--------------------------
%% Project - Task Manager
%% 
%% Project Information
%% https://github.com/tja58/Erlang-task_manager
%%--------------------------

%%--------------------------
%% Spawn Process
%%--------------------------
start(Init_task_list) ->
    Pid = spawn(?MODULE, run, [Init_task_list]),
    register(run_reg, Pid),
    Pid.

%%--------------------------
%% Client functions
%%--------------------------
rpc(Pid, Message) ->
    Pid ! {self(), Message},
    receive
        Response ->
            Response
    end.

%%--------------------------
%% Server functions
%%--------------------------
run(Task_list) ->
    Updated_Task_list =
        receive
            %%--------------------------
            %% Add Task(s)
            %%--------------------------
            % Add Tasks
            {From, {add, Tasks}} when is_list(Tasks) ->
                From ! added,
                Added_Tasks = lists:map(fun(Task) -> add_task(Task) end, Tasks),
                Added_Tasks ++ Task_list;
            % Add Task
            {From, {add, Task}} ->
                From ! added,
                Added_Task = add_task(Task),
                [Added_Task] ++ Task_list;
            %%--------------------------
            %% Complete Task(s)
            %%--------------------------
            % Complete Task(s)
            {From, {complete, Task}} ->
                From ! tasks_completed,
                complete_task(Task, Task_list);
            %%--------------------------
            %% Remove Task(s)
            %%--------------------------
            {From, {remove, Task}} ->
                From ! removed,
                remove(Task, Task_list);
            %%--------------------------
            %% Remove Completed Task(s)
            %%--------------------------
            {From, rm_completed} ->
                From ! removed_completed,
                remove_complete(Task_list);
            %%--------------------------
            %% Get Task
            %%--------------------------
            % Get Tasks
            {From, get} ->
                print_get(Task_list),
                From ! Task_list
        end,
    run(Updated_Task_list).

%%--------------------------
%% Helper functions
%%--------------------------

%%--------------------------
%% Add Task(s) functions
%%--------------------------

% Add Task; Case Not A Tuple
add_task(Task) when is_tuple(Task) == false ->
    % Initialize Vars
    Completed = {completed, false},
    Desc = {description, none},

    % Return Data
    {Task, [Completed, Desc]};
% Add Task; Case Tuple
add_task(Task) ->
    % Indexing Task
    Task_name = element(1, Task),
    Task_Desc = element(2, Task),

    % Formatting Vars
    Completed = {completed, false},
    Desc = {description, Task_Desc},

    {Task_name, [Completed, Desc]}.

%%--------------------------
%% Complete Task(s) functions
%%--------------------------

% Complete Task; When Atom
complete_task(Task, Task_list) when is_atom(Task) ->
    % Searches the list for task
    case lists:keysearch(Task, 1, Task_list) of
        % If task is in list
        {value, {Task, Task_data}} ->
            % Replace the complete to value to true
            Updated_task_data = lists:keyreplace(completed, 1, Task_data, {completed, true}),
            % Create new tuple with Task name & Updated Task Data
            Updated_task = {Task, Updated_task_data},
            % Replace the task in the list with the updated task
            lists:keyreplace(Task, 1, Task_list, Updated_task);
        % If task is not in list
        false ->
            % Return Task list
            Task_list
    end;
% Complete Task; When A List
complete_task(Tasks, Task_list) when is_list(Tasks) ->
    % Map over state list
    lists:map(fun({Task, Task_data}) ->
                 % Check to see if list of tasks inputed is in state list
                 case lists:member(Task, Tasks) of
                     % Task in state; update completed to true
                     true -> {Task, lists:keyreplace(completed, 1, Task_data, {completed, true})};
                     % Task not in state; return task tuple
                     false -> {Task, Task_data}
                 end
              end,
              Task_list);
% Complete Task; When not list or atom
complete_task(_Task, Task_list) ->
    Task_list.

%%--------------------------
%% remove function; Remove a task
%%--------------------------
remove_helper({Task, _}, Tasks) ->
    lists:member(Task, Tasks).

remove(Task, Task_list) when is_atom(Task) ->
    lists:keydelete(Task, 1, Task_list);
remove(Tasks, Task_list) when is_list(Tasks) ->
    lists:filter(fun(Task) -> not remove_helper(Task, Tasks) end, Task_list);
remove(_Tasks, Task_list) ->
    Task_list.

%%--------------------------
%% remove_complete function; Remove all tasks that are completed
%%--------------------------
remove_complete(Task_list) when is_list(Task_list) ->
    lists:filter(fun({_, Task_data}) -> not completed_value(Task_data) end, Task_list);
remove_complete(Task_list) ->
    Task_list.

%%--------------------------
%% Print_get function; Format Terminal print
%%--------------------------
print_get(Task_list) ->
    io:format("~n"),
    lists:foreach(fun({Task_name, Task_data}) ->
                     io:format("Task Name: ~p~nCompleted: ~p~nDescription: ~p~n~n",
                               [Task_name,
                                completed_value(Task_data),
                                description_value(Task_data)])
                  end,
                  Task_list),
    Task_list.

% completed_value; Get completed_value and return to print_get
completed_value(Task_data) ->
    case lists:keysearch(completed, 1, Task_data) of
        {value, {completed, Value}} ->
            Value;
        false ->
            false  % Handle the case when 'completed' is not found
    end.

% description_value; Get description_value and return to print_get
description_value(Task_data) ->
    case lists:keysearch(description, 1, Task_data) of
        {value, {description, Value}} ->
            Value;
        false ->
            undefined  % Handle the case when 'description' is not found
    end.

%%--------------------------
%% EUNIT Tests
%%--------------------------
-ifdef(EUNIT).

-include_lib("eunit/include/eunit.hrl").

%% Add Task Tests
add_task_test_() ->
    {setup,
     fun() ->
        % Runs before any of the tests
        % inital Vals
        Init =
            [{task1, [{completed, false}, {description, none}]},
             {task2, [{completed, false}, {description, none}]},
             {task3, [{completed, false}, {description, none}]}],
        Pid = spawn(?MODULE, run, [Init]),
        register(test_adder, Pid)
     end,
     fun(_) ->
        % Runs after any of the tests
        unregister(test_adder)
     end,
     [% Add tests start here
      ?_assertEqual(added, rpc(test_adder, {add, get_groceries})),
      ?_assertEqual(added, rpc(test_adder, {add, get_gas})),
      ?_assertEqual(added, rpc(test_adder, {add, pick_up_kids})),
      ?_assertEqual(added, rpc(test_adder, {add, {do_homework, complete_cse121_project}}))]}.

%% Add Tasks Tests
add_tasks_test_() ->
    {setup,
     fun() ->
        % Runs before any of the tests
        Init =
            [{task1, [{completed, false}, {description, none}]},
             {task2, [{completed, false}, {description, none}]},
             {task3, [{completed, false}, {description, none}]}],
        Pid = spawn(?MODULE, run, [Init]),
        register(test_adder, Pid)
     end,
     fun(_) ->
        % Runs after any of the tests
        unregister(test_adder)
     end,
     [% Add tests start here
      ?_assertEqual(added, rpc(test_adder, {add, []})),
      ?_assertEqual(added,
                    rpc(test_adder, {add, [get_homework_done, go_to_work, get_dog_food]})),
      ?_assertEqual(added,
                    rpc(test_adder,
                        {add, [{task3, task3_description}, {task4, task4_description}]}))]}.

%% Complete Task Tests
complete_task_test_() ->
    {setup,
     fun() ->
        % Runs before any of the tests
        Init =
            [{task1, [{completed, false}, {description, none}]},
             {task2, [{completed, false}, {description, none}]},
             {task3, [{completed, false}, {description, none}]}],
        Pid = spawn(?MODULE, run, [Init]),
        register(test_complete, Pid)
     end,
     fun(_) ->
        % Runs after any of the tests
        unregister(test_complete)
     end,
     [% Add tests start here
      ?_assertEqual(tasks_completed, rpc(test_complete, {complete, []})),
      ?_assertEqual(tasks_completed, rpc(test_complete, {complete, task1})),
      ?_assertEqual(tasks_completed, rpc(test_complete, {complete, task2}))]}.

%% Complete Tasks Tests
complete_tasks_test_() ->
    {setup,
     fun() ->
        % Runs before any of the tests
        Init =
            [{task1, [{completed, false}, {description, none}]},
             {task2, [{completed, false}, {description, none}]},
             {task3, [{completed, false}, {description, none}]}],
        Pid = spawn(?MODULE, run, [Init]),
        register(test_complete, Pid)
     end,
     fun(_) ->
        % Runs after any of the tests
        unregister(test_complete)
     end,
     [% Add tests start here
      ?_assertEqual(tasks_completed, rpc(test_complete, {complete, []})),
      ?_assertEqual(tasks_completed, rpc(test_complete, {complete, [task1, task2]})),
      ?_assertEqual(tasks_completed, rpc(test_complete, {complete, [task3]})),
      ?_assertEqual(tasks_completed, rpc(test_complete, {complete, [tasks3, task4]}))]}.

%% Remove Task Tests
remove_task_test_() ->
    {setup,
     fun() ->
        % Runs before any of the tests
        Init =
            [{task1, [{completed, false}, {description, none}]},
             {task2, [{completed, false}, {description, none}]},
             {task3, [{completed, false}, {description, none}]}],
        Pid = spawn(?MODULE, run, [Init]),
        register(test_remove, Pid)
     end,
     fun(_) ->
        % Runs after any of the tests
        unregister(test_remove)
     end,
     [% Add tests start here
      ?_assertEqual(removed, rpc(test_remove, {remove, task1})),
      ?_assertEqual(removed, rpc(test_remove, {remove, task2})),
      ?_assertEqual(removed, rpc(test_remove, {remove, task3})),
      ?_assertEqual(removed, rpc(test_remove, {remove, task4}))]}.

%% Remove Tasks Tests
remove_tasks_test_() ->
    {setup,
     fun() ->
        % Runs before any of the tests
        Init =
            [{task1, [{completed, false}, {description, none}]},
             {task2, [{completed, false}, {description, none}]},
             {task3, [{completed, false}, {description, none}]}],
        Pid = spawn(?MODULE, run, [Init]),
        register(test_remove, Pid)
     end,
     fun(_) ->
        % Runs after any of the tests
        unregister(test_remove)
     end,
     [% Add tests start here
      ?_assertEqual(removed, rpc(test_remove, {remove, []})),
      ?_assertEqual(removed, rpc(test_remove, {remove, [task2, task3]})),
      ?_assertEqual(removed, rpc(test_remove, {remove, [task1, task3]})),
      ?_assertEqual(removed, rpc(test_remove, {remove, [task4]}))]}.

%% Print Tasks Tests
print_tasks_test_() ->
    {setup,
     fun() ->
        % Runs before any of the tests
        Init =
            [{task1, [{completed, false}, {description, none}]},
             {task2, [{completed, false}, {description, none}]},
             {task3, [{completed, false}, {description, none}]}],
        Pid = spawn(?MODULE, run, [Init]),
        register(test_get, Pid)
     end,
     fun(_) ->
        % Runs after any of the tests
        unregister(test_get)
     end,
     % Add tests start here
     [?_assertEqual([{task1, [{completed, false}, {description, none}]},
                     {task2, [{completed, false}, {description, none}]},
                     {task3, [{completed, false}, {description, none}]}],
                    rpc(test_get, get))]}.

-endif.
