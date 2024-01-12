# Project - Task Manager

## Purpose
The purpose of this project is to demonstrate my knowledge I have gained of Erlang. In this example we can see the use of Erlang to help sort out tasks that the user needs to accomplish.

## Data Structure
The structure of the data is a list of tuples to store the data of each task. In each tuple there are two elements, the first element is the name of the task, and the second element is a list of data about the task. The first index of the data lists is the completed status. The status of completed will be either true or false. The second index of the data list is the task description which will be either set to none, or a description the user inputs. 

An example of the struture can be seen below

    % Example structure of 3 tasks.
    [
        {task1, [{completed, false}, {description, none}]},
        {task2, [{completed, false}, {description, none}]},
        {task3, [{completed, false}, {description, none}]}
    ]

Code to get this result

    Pid = project:start([]),
    project:rpc(Pid, {add, [task1, task2, task3]}).

## Functions

#### Initializing
##### Purpose
The purpose of initializing is to get the process id so the calls to the server can know where the data needs to be sent.

##### Example
    % Initialize with empty array
    Pid = project:start([]).

    % The empty array sent in to the start can be filled with data
    % The structure of the data must apply to the same structure of the data
    % Example of this is seen below

    Init = 
        [{task1, [{completed, false}, {description, none}]},
        {task2, [{completed, false}, {description, none}]},
        {task3, [{completed, false}, {description, none}]}],
    Pid = project:start(Init).

#### Add Task
##### Purpose
The purpose of this function is to add an element to the task list. This can be done either by entering an atom or tuple into the call of this function.

##### Examples

    % Inputs

    % 1. Atom
    project:rpc(Pid, {add, task4}).

    % 2. Tuple
    project:rpc(Pid, {add, {task5, task5_description}}).

    % Outputs

    % 1. Atom
    added

    % 2. Tuple
    added

#### Add Tasks
##### Purpose
The purpose of this function is to add multiple tasks to the task list. This can be done by entering a list of atoms, tuples, or both into the call of this function.

##### Examples

    % Inputs

    % 1. List of atoms
    project:rpc(Pid, {add, [task6, task7]}).

    % 2. List of tuples
    project:rpc(Pid, {add, [{task8, task8_description},{task9, task9_description}]}).

    % 3. List of both atoms and tuples
    project:rpc(Pid, {add, [task10, {task11, task11_description}]}).

    % Outputs

    % 1. List of atoms
    added 

    % 2. List of tuples
    added

    % 3. List of both atoms and tuples
    added

#### Complete Task
##### Purpose
The purpose of this function is to mark a task as complete. The inputted data should be formatted as a atom.

##### Example
    % Input
    project:rpc(Pid, {complete, task1}).

    % Output
    tasks_completed


#### Complete Tasks
##### Purpose
The purpose of this function is to mark multiple tasks as complete. The inputted data should be formatted as a list of atoms.
##### Examples
    % Input
    project:rpc(Pid, {complete, [task2, task3, task4]}).

    % Output
    tasks_completed

#### Remove Task
##### Purpose
The purpose of this function is to remove a task from the list. The inputted data should be formatted as a atom.
##### Examples
    % Input
    project:rpc(Pid, {remove, task1}).

    % Output
    removed

#### Remove Tasks
##### Purpose
The purpose of this function is to remove multiple tasks from the task list. The inputted data should be formatted as a list of atoms.
##### Examples
    % Input
    project:rpc(Pid, {remove, [task2, task3, task4]}).

    % Output
    removed

#### Remove Completed Tasks
##### Purpose
The purpose of this function is to remove all of the completed tasks from the task lists. There is no data that should be sent in this call.
##### Examples
    % Input
    project:rpc(Pid, rm_completed).

    % Output
    removed_completed

#### Get Tasks
##### Purpose
The purpose of this function is to get all of the current tasks in the list. There is no data that should be sent in this call.
##### Examples
    % Input
    project:rpc(Pid, get).

    % Output
    % Task List should be returned, and a formatted list of the list will appear in console. 
    % However, this can be removed by commenting out the line "print_get(Task_list)"

## Running Tests
In order to run the tests for this program Rebar3 Eunit should be used. The tests will be shown at the bottom of the file. The code below shows how to run the tests.

    rebar3 eunit

## Live Example

    1> Init = 
    1>  [{task1, [{completed, false}, {description, none}]},
    1>  {task2, [{completed, false}, {description, none}]},
    1>  {task3, [{completed, false}, {description, none}]}],
    1> Pid = project:start(Init).
    <0.---.0> % Pid returned

    2> project:rpc(Pid, {add, task4}).
    added

    3> project:rpc(Pid, {add, {task5, task5_description}}).
    added

    4> project:rpc(Pid, {add, [task6, task7]}).
    added

    5> project:rpc(Pid, {add, [{task8, task8_description},{task9, task9_description}]}).
    added

    6> project:rpc(Pid, {add, [task10, {task11, task11_description}]}).
    added

    7> project:rpc(Pid, {complete, task1}).
    tasks_completed

    8> project:rpc(Pid, {complete, [task2, task3, task4]}).
    tasks_completed

    9> project:rpc(Pid, {complete, [task7, task8, task9]}).
    tasks_completed

    10> project:rpc(Pid, {remove, task1}).
    removed

    11> project:rpc(Pid, {remove, [task2, task3, task4]}).
    removed

    12> project:rpc(Pid, get).

    Task Name: task10
    Completed: false
    Description: none

    Task Name: task11
    Completed: false
    Description: task11_description

    Task Name: task8
    Completed: true
    Description: task8_description

    Task Name: task9
    Completed: true
    Description: task9_description

    Task Name: task6
    Completed: false
    Description: none

    Task Name: task7
    Completed: true
    Description: none

    Task Name: task5
    Completed: false
    Description: task5_description

    [{task10,[{completed,false},{description,none}]},
    {task11,[{completed,false},{description,task11_description}]},
    {task8,[{completed,true},{description,task8_description}]},
    {task9,[{completed,true},{description,task9_description}]},
    {task6,[{completed,false},{description,none}]},
    {task7,[{completed,true},{description,none}]},
    {task5,[{completed,false},{description,task5_description}]}]

    13> project:rpc(Pid, rm_completed).
    removed_completed

    14> project:rpc(Pid, get).
    
    Task Name: task10
    Completed: false
    Description: none

    Task Name: task11
    Completed: false
    Description: task11_description

    Task Name: task6
    Completed: false
    Description: none

    Task Name: task5
    Completed: false
    Description: task5_description

    [{task10,[{completed,false},{description,none}]},
    {task11,[{completed,false},{description,task11_description}]},
    {task6,[{completed,false},{description,none}]},
    {task5,[{completed,false},{description,task5_description}]}]
    