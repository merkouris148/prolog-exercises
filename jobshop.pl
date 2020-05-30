%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This code has been tested in ECLiPSe Prolog Version 7.0
% and SWI-Prolog Version 7.6.4.
%
% with the example data set, we had cpu time in ECLiPSe:
% Comand															Time
% --------------------------------------------------------------------------------------------------
% jobshop(S).														4.04s
% findall(S, jobshop(S), L), length(L, N).							173.90s
% jobshop_with_manpower(S).											6.98s
% findall(S, jobshop_with_manpower(S), L), length(L, N).			174.92s
%
% Author: Merkouris Papamichail
% email: sdi1400148@di.uoa.gr
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% !!!! READ ME !!!!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*	ΠΑΡΑΔΟΧΕΣ ΩΣ ΠΡΟΣ ΤΗΝ ΜΟΡΦΗ ΤΩΝ ΔΕΔΟΜΕΝΩΝ
* Τόσο το jobshop/1, όσο και το jobshop_with_manpower/1
* προϋποθέτουν να δοθούν τα δεδομένα ως task/4, και ΟΧΙ
* task/3. Αν έχουμε δεδομένα στην μορφή task/4 και καλέσουμε
* την jobshop/1, αυτή απλώς δεν θα λάβει υπόψη της τον
* περιορισμό για τους εργαζόμανους και θα μας βγάλει σωστά
* τις λύσεις.
* Αν παρ' όλα αυτά καλέσουμε το πρόγραμμα με task/3 ως
* δεδομένα, τότε για να λειτουργήσει σωστά το πρόγραμμα
* πρέπει να αποσχολιαστεί η παρακάτω γραμμή:
*/

% task(Id, Machine, Duration, 0) :- task(Id, Machine, Duration).

/* 
* Υποθέτουμε ότι το πρώτο όρισμα στο task/4, το TaskId είναι
* ΜΟΝΑΔΙΚΟ για κάθε εργασία.
*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Example Data
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
job(j1,[t11,t12]).
job(j2,[t21,t22,t23]).
job(j3,[t31]).
job(j4,[t41,t42]).

task(t11,m1,2,3).	% task(t11,m1,2).
task(t12,m2,6,2).	% task(t12,m2,6).

task(t21,m2,5,2).	% task(t21,m2,5).
task(t22,m1,3,3).	% task(t22,m1,3).
task(t23,m2,3,2).	% task(t23,m2,3).

task(t31,m2,4,2).	% task(t31,m2,4).

task(t41,m1,5,4).	% task(t41,m1,5).
task(t42,m2,2,1).	% task(t42,m2,2).

machine(m1,1).
machine(m2,2).

deadline(14).

staff(6).
*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%    Utilities    %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%
% List Maniplulation %
%%%%%%%%%%%%%%%%%%%%%%


% multilist/3
% Use: multilist(+Atom, +N, -MultiList)
% (Atom, N) --> MultiList = [Atom, Atom, ..., Atom], |MultiList| = N
multilist(_, 0, []) :- !.
multilist(Atom, N, [Atom| List]) :-
	M is N-1, M >= 0,
	multilist(Atom, M, List).

% list_of_multilist/2
% Use: list_of_multilist(+Args, -MultiLists)
% [[Atom, N] | Args] --> [[Atom, Atom, ..., Atom] | MultiLists]
list_of_multilist([], []).
list_of_multilist([[Atom, N] | Args], [MultiList | MultiLists]) :-
	multilist(Atom, N, MultiList),
	list_of_multilist(Args, MultiLists).

% sublist/2
% Use: sublist(?S, +L)
% true if S is a sublist of L
sublist(S, L) :-
   append(_, L2, L),
   append(S, _, L2).

delete_duplicates([], []).
delete_duplicates([L|List], Set) :-
	member(L, List),
	delete_duplicates(List, Set).
delete_duplicates([L|List], [L|Set]) :-
	\+ member(L, List),
	delete_duplicates(List, Set).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%    Queries    %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prev_task(TaskId, PrevTaskId) :-
	task_job(TaskId, Job),
	job(Job, TaskList),
	find_prev_task(TaskList, TaskId, PrevTaskId).

find_prev_task([PrevTaskId, TaskId|_], TaskId, PrevTaskId) :- !.
find_prev_task([_, OtherTaskId | TaskList], TaskId, PrevTaskId) :-
	OtherTaskId \= TaskId,
	find_prev_task([OtherTaskId|TaskList], TaskId, PrevTaskId).

task_job(Task, Job) :-
	job(Job, Tasks),
	member(Task, Tasks).

get_all_jobs(Jobs) :-
	findall(J, job(J, _), Jobs).


% get_machine_bag/1
% Use: get_machine_bag(-Machines)
% Returns a multiset of all the machines available
get_machine_bag(Machines) :-
	get_all_machine_types(MachineTypes),
	get_machine_bag(MachineTypes, Machines).

% get_all_machine_types/1
% Use: get_machine_bag(-MachineTypes)
get_all_machine_types(MachineTypes) :-
	findall(MachineType, task(_,MachineType, _, _), MultiMachineTypes),
	delete_duplicates(MultiMachineTypes, MachineTypes),!.

% get_machine_bag/2
% Use: machine_bag(+MachineTypes, -Machines)
get_machine_bag([], []) :- !.
get_machine_bag([MachineType|MachineTypes], Machines) :-
	machine(MachineType, N),
	multilist(MachineType, N, Machines1),	
	get_machine_bag(MachineTypes, Machines2),
	append(Machines1, Machines2, Machines).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%    Queue   %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% create_queue/1
% Use: create_queue(-Queue)
create_queue(Queue) :-
	findall([Task, Duration], task(Task, _, Duration, _), Queue_tmp),
	list_of_multilist(Queue_tmp, Queue).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%    Schedule   %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

initialize_TT(EmptyTimeTable) :-
	deadline(D),
	get_machine_bag(Ms),
	initialize_TT(D, Ms, EmptyTimeTable).

initialize_TT(_, [], []).
initialize_TT(D, [M|Ms], [TT|TimeTables]) :-
	length(L, D),
	TT = execs(M, L),
	initialize_TT(D, Ms, TimeTables).

generate_TT(TT) :-
	create_queue(Q),
	initialize_TT(TT),
	generate_TT(Q, TT).

generate_TT([], _).
generate_TT([Task|Tasks], TT) :-			% get the first task
	Task = [T|_], task(T, MT, _, _),		% get the machine of the first task
	member(execs(MT, Schedule), TT),		% get the correct Shcedule
	sublist(Task, Schedule),				% schedule the task
	generate_TT(Tasks, TT). 

beutify_Schedule([], _, t(nul, -1, -1), []).													% base case 1
beutify_Schedule([], N, t(TaskId, StartTime, -1), [t(TaskId, StartTime, EndTime)]) :-			% base case 2
	TaskId \= nul,
	StartTime \= -1,
	EndTime is N - 1.
beutify_Schedule([S|SchedIn], N, t(nul, -1, -1), SchedOut) :-									% skipping empty positions
	var(S),
	N1 is N + 1,
	beutify_Schedule(SchedIn, N1, t(nul, -1, -1),  SchedOut).
beutify_Schedule([S|SchedIn], N, t(TaskId, StartTime, -1), SchedOut) :-							% finding the first empty position
	var(S),
	TaskId \= nul,
	StartTime \= -1,
	EndTime is N - 1,
	N1 is N + 1,
	beutify_Schedule(SchedIn, N1, t(nul, -1, -1), SchedOut1),
	append([t(TaskId, StartTime, EndTime)], SchedOut1, SchedOut).
beutify_Schedule([S|SchedIn], N, t(TaskId, StartTime, -1), SchedOut) :-							% a different tasks starts right next the previous one
	nonvar(S), S \= TaskId,
	TaskId \= nul,
	StartTime \= -1,
	EndTime is N - 1,
	N1 is N + 1,
	beutify_Schedule(SchedIn, N1, t(S, N, -1), SchedOut1),
	append([t(TaskId, StartTime, EndTime)], SchedOut1, SchedOut).
beutify_Schedule([S|SchedIn], N, t(nul, -1, -1), SchedOut) :-									% finding a new first non empty position
	nonvar(S),
	N1 is N + 1,
	beutify_Schedule(SchedIn, N1, t(S, N, -1), SchedOut).
beutify_Schedule([S|SchedIn], N, t(TaskId, StartTime, -1), SchedOut) :-							% skipping the tokens in the current task
	nonvar(S),
	S = TaskId,
	TaskId \= nul,
	StartTime \= -1,
	N1 is N + 1,
	beutify_Schedule(SchedIn, N1, t(TaskId, StartTime, -1), SchedOut).

% going from descrete time slots in real time
beutify_Schedule2([], []).
beutify_Schedule2([t(TaskId, StartTime, EndTime)|ScheduleIn], [t(TaskId, StartTime, NewEndTime)|SchedOut]) :-
		NewEndTime is EndTime + 1,
		beutify_Schedule2(ScheduleIn, SchedOut).

beutify_Schedule_complete(ScheduleIn, [t(TaskId, StartTime, NewEndTime)|ScheduleOut]) :-
	beutify_Schedule(ScheduleIn, 0, t(nul, -1, -1), [t(TaskId, StartTime, EndTime)|Schedule_tmp]),
	NewEndTime is EndTime + 1,
	beutify_Schedule2(Schedule_tmp, ScheduleOut).

beutify_TT([], []).
beutify_TT([execs(MT, ScheduleIn)|TTIn], [execs(MT, ScheduleOut)|TTOut]) :-
	beutify_Schedule_complete(ScheduleIn, ScheduleOut),
	beutify_TT(TTIn, TTOut).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%    Validation   %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% given a Time Table TT and a TaskId, it returns when the task
% is scheduled
scheduled(TT, TaskId, t(TaskId, TaskStartTime, TaskEndTime)) :-
	task(TaskId, MT, _, _),
	member(execs(MT, Schedule), TT),
	member(t(TaskId, TaskStartTime, TaskEndTime), Schedule).

% checking if a task is scheduled after its previous in the job
feasible(t(TaskId, TaskStartTime, _), TT) :-
	prev_task(TaskId, PrevTaskId),
	scheduled(TT, PrevTaskId, t(_, _, PrevTaskEndTime)),
	PrevTaskEndTime =< TaskStartTime.
feasible(t(TaskId, _, _), _) :-
	\+ prev_task(TaskId, _).

validate_Schedule([], _).
validate_Schedule([Task|Schedule], TT_const) :-
	feasible(Task, TT_const),
	validate_Schedule(Schedule, TT_const).

validate_TT(TT) :-
	validate_TT(TT, TT).

validate_TT([], _).
validate_TT([execs(_, Schedule)|TT], TT_const) :-
	validate_Schedule(Schedule, TT_const),
	validate_TT(TT, TT_const).

jobshop(TTOut) :-
	generate_TT(TT),		% generate
	beutify_TT(TT, TTOut),
	validate_TT(TTOut).		% & test



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%    Tasks With Manpower   %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% checks if in every "second" there are only staff(Workers), working
feasible_workers(_, _, 0).
feasible_workers(TT, TotalWorkers, Time) :-
	Time > 0,
	findall(
				t(Id, S, E),
				(
					task(Id, _, _, _),
					scheduled(TT, Id, t(Id, S, E)),
					S =< Time, Time < E
				),
				Tasks
	),
	validate_Workers(Tasks, TotalWorkers),
	NewTime is Time - 1,
	feasible_workers(TT, TotalWorkers, NewTime).
feasible_workers(TT, TotalWorkers, Time) :-
	Time > 0,
	\+ findall(
				t(Id, S, E),
				(
					task(Id, _, _, _),
					scheduled(TT, Id, t(Id, S, E)),
					S =< Time, Time < E
				),
				_
	),
	NewTime is Time - 1,
	feasible_workers(TT, TotalWorkers, NewTime).

validate_Workers([], AvailableWorkers) :-
	AvailableWorkers >= 0.
validate_Workers([t(Id, _, _)|Tasks], AvailableWorkers) :-
	AvailableWorkers > 0,
	task(Id, _, _, WorkersNeeded),
	NewAvailableWorkers is AvailableWorkers - WorkersNeeded,
	validate_Workers(Tasks, NewAvailableWorkers).

jobshop_with_manpower(TT) :-
	staff(TotalWorkers),
	deadline(Time),
	jobshop(TT),								% generate
	feasible_workers(TT, TotalWorkers, Time).	% & test
