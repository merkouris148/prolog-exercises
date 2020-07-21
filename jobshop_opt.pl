%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This code has been tested in ECLiPSe Prolog Version 7.0
%
% Testing the example data produced the following results:
%
%	Queries, Times and Costs:
%--------------------------------------------------------------------------------------------------
% 1]	Q: jobshop_opt([j1,j2,j3,j4,j5], 10, Schedule, Cost, 1.0, 0).
% 		T: 0.12s
%		C: 12
%
% 2]	Q: jobshop_opt([j1,j2,j3,j4,j5], 7, Schedule, Cost, 1.0, 0).
%		T: 2.55s
%		C: 13
%
% 3]	Q: jobshop_opt([j1,j2,j3,j4], 6, Schedule, Cost, 1.0, 0).
%		T: 21.68s
%		C: 14
%
% 4]	Q: jobshop_opt([j1,j2,j3,j4], 5, Schedule, Cost, 1.0, 20).
%		T: 20.01s
%		C: 16
%
% 5]	Q: jobshop_opt([j1,j2], 3, Schedule, Cost, 1.0, 0).
%		T: 5.37s
%		C: 19
%
% 6]	Q: jobshop_opt([j1,j2], 2, Schedule, Cost, 1.0, 0).
%		T: 0.01s
% 		C: No Solution
%
% 7]	Q: jobshop_opt([j1,j4,j5], 3, Schedule, Cost, 1.0, 0).
%		T: 3.16s
%		C: 15
%
% 8]	Q: jobshop_opt([j1,j4,j5], 3, Schedule, Cost, 3.0, 0).
%		T: 0.75s
%		C: 16
%
% 9]	Q: jobshop_opt([j1,j2,j3,j4,j5], 6, Schedule, Cost, 2.0, 0).
%		T: 21.08s
%		C: 15
%
% Author: Merkouris Papamichail
% email: sdi1400148@di.uoa.gr
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/**************************************************************************************************
 **** READ ME *************************************************************************************
 **************************************************************************************************
 * In this implementation we use the following compound
 * terms as variables:
 *
 * A. for every task, let:
 *		execs(
 *				TaskId,			% constant
 *				MachineType,	% constant
 *				SerialNo,		% constraint variable
 *				StartTime,		% constraint variable
 *				EndTime			% constraint variable
 *		)
 * where:
 * 1.	EndTime = StartTime + Duration
 * 2.	if PrevTask is the previous task of Task in a job,
 * 		we demand:
 *
 *			PrevEndTime =< TaskStratTime
 *
 * 3.	if Task1, Task2 are executed in the same machine
 *		(MachineType1 = MachineType2
 *		and SerialNo1 = SerialNo2), we demand:
 *
 *			EndTime1 =< StartTime2
 *			or EndTime2 =< StartTime1
 *
 *		That is, the execution of the two task cannot
 *		overlap.
 *
 * B. for every Sec in {0, ..., MaxTime}, for every machine
 *	and serial number, let:
 *		time_slot(
 *					MachineType,		% constant
 *					SerialNo,			% constant
 *					Sec,				% constant
 *					OccupiedWorkes		% constant variable
 *		)
 *	where:
 *	1.	if there is a task, where:
 *			StartTime =< Sec < EndTime
 *		then
 *			OccupiedWorkes = TaskWorkers
 *
 * C. for every Sec in {0, ..., MaxTime}, let:
 *		time_slice(
 *					Sec,				% constant
 *					OccupiedWorkes		% constraint variable
 *		)
 * 	where for a fixed Sec
 *	1.		OccupiedWorkes = sum({SlotOccupiedWorkes : time_slot(_, _, Sec, SlotOccupiedWorkes)})
 *	2.		OccupiedWorkes =< Staff
 **************************************************************************************************/

:- compile(jobshop_opt_data).	% to compile the example data uncomment THIS line
:- lib(ic).
:- lib(ic_edge_finder).
:- lib(branch_and_bound).
:- lib(listut).
:- set_flag(print_depth,1000).

%%%%%%%%%%%%%%%%%%%%%%
% List Maniplulation %
%%%%%%%%%%%%%%%%%%%%%%

delete_duplicates([], []).
delete_duplicates([L|List], Set) :-
	member(L, List),
	delete_duplicates(List, Set).
delete_duplicates([L|List], [L|Set]) :-
	\+ member(L, List),
	delete_duplicates(List, Set).

% filter/3
% Use: filter(+List, +Patern, -Gather).
% Gather = {L \in List : L is unifiable with Patern}
filter([], _, []).
filter([L|List], Patern, [L|Gather]) :-
	\+ L \= Patern, 						% a bit hacky way to check if two terms are unifiable, without unifying them
	filter(List, Patern, Gather).
filter([L|List], Patern, Gather) :-
	L \= Patern,
	filter(List, Patern, Gather).

%%%%%%%%%%%%%
% Utilities %
%%%%%%%%%%%%%

task_job(Task, Job) :-
	job(Job, Tasks),
	member(Task, Tasks).

gather_tasks([], []).
gather_tasks([Job|Jobs], Tasks) :-
	job(Job, Tasks1),
	gather_tasks(Jobs, Tasks2),
	append(Tasks1, Tasks2, Tasks).

gather_endtime([], []).
gather_endtime([execs(_, _, _, _, EndTime)|Vars], [EndTime|EndTimes]) :-
	gather_endtime(Vars, EndTimes).

gather_variables(Schedule, TimeSlots, TimeSlices, Vars) :-
	gather_schedule_variables(Schedule, ScheduleVars),
	gather_time_slots_variables(TimeSlots, TimeSlotsVars),
	gather_time_slices_variables(TimeSlices, TimeSlicesVars),
	append(ScheduleVars, TimeSlotsVars, Temp),
	append(Temp, TimeSlicesVars, Vars).

gather_time_slices_variables([], []).
gather_time_slices_variables(
								[time_slice(_, OccupiedWorkes)|TimeSlices],
								[OccupiedWorkes|Vars]
							) :-
	gather_time_slices_variables(TimeSlices, Vars).

gather_time_slots_variables([], []).
gather_time_slots_variables(
								[time_slot(_, _, _, OccupiedWorkes)|TimeSlots],
								[OccupiedWorkes|Vars]
							) :-
	gather_time_slots_variables(TimeSlots, Vars).

gather_schedule_variables([], []).
gather_schedule_variables(
							[execs(_, _, SerialNo, StartTime, EndTime)|Schedule],
							[SerialNo, StartTime, EndTime|Vars]
						) :-
	gather_schedule_variables(Schedule, Vars).

gather_worker_by_sec(_, [], []).
gather_worker_by_sec(Sec, [time_slot(_, _, Sec, OccupiedWorkes)|TimeSlots], [OccupiedWorkes|SecTimeSlots]) :-
	gather_worker_by_sec(Sec, TimeSlots, SecTimeSlots).
gather_worker_by_sec(Sec1, [time_slot(_, _, Sec2, _)|TimeSlots], SecTimeSlots) :-
	Sec1 \= Sec2,
	gather_worker_by_sec(Sec1, TimeSlots, SecTimeSlots).

% get_all_machine_types/1
% Use: get_machine_bag(-MachineTypes)
get_all_machine_types(MachineTypes) :-
	findall(MachineType, machine(MachineType, _), MachineTypes).

get_all_machine_instances(MachineTypeInst) :-
	findall(Instances, machine(_, Instances), MachineTypeInst).

calculate_max_time([], 0).
calculate_max_time([Task|Tasks], MaxTime) :-
	task(Task, _, Duration, _),
	calculate_max_time(Tasks, PartTime),
	MaxTime is PartTime + Duration.

prev_task(TaskId, PrevTaskId) :-
	task_job(TaskId, Job),
	job(Job, TaskList),
	find_prev_task(TaskList, TaskId, PrevTaskId).

find_prev_task([PrevTaskId, TaskId|_], TaskId, PrevTaskId) :- !.
find_prev_task([_, OtherTaskId | TaskList], TaskId, PrevTaskId) :-
	OtherTaskId \= TaskId,
	find_prev_task([OtherTaskId|TaskList], TaskId, PrevTaskId).

beautify(Schedule, BeautySchedule) :-
	initialize_beautify(TempSchedule1),
	fill_beautify(TempSchedule1, Schedule, TempSchedule2),
	beautify_conclude(TempSchedule2, BeautySchedule).

initialize_beautify(TempSchedule) :-
	get_all_machine_types(MachineTypes),
	get_all_machine_instances(MachineTypeInst),
	make_tmp_sched(MachineTypes, MachineTypeInst, TempSchedule).

make_tmp_sched([], [], []).
make_tmp_sched([MT|MachineTypes], [N|MachineTypeInst], TempSchedule) :-
	make_tmp_sched2(MT, N, TempSchedule1),
	make_tmp_sched(MachineTypes, MachineTypeInst, TempSchedule2),
	append(TempSchedule1, TempSchedule2, TempSchedule).

make_tmp_sched2(_, 0, []).
make_tmp_sched2(MachineType, N, [execs(MachineType, N, [])|TempSchedule]) :-
	N > 0,
	M is N - 1,
	make_tmp_sched2(MachineType, M, TempSchedule).

fill_beautify([], [], []).
fill_beautify([execs(MachineType, N, [])|TempSchedule1], Schedule, [execs(MachineType, Gather)|TempSchedule2]) :-
	filter(Schedule, execs(_, MachineType, N, _, _), Gather),
	subtract(Schedule, Gather, Schedule1),
	fill_beautify(TempSchedule1, Schedule1, TempSchedule2).

beautify_conclude([], []).
beautify_conclude([execs(MachineType, Tasks)|TempSchedule], [execs(MachineType, BeautyTasks)|BeautySchedule]) :-
	beautify_conclude_h(Tasks, BeautyTasks),
	beautify_conclude(TempSchedule, BeautySchedule).

beautify_conclude_h([], []).
beautify_conclude_h(
						[execs(Task, _, _, StartTime, EndTime)|Tasks],
						[t(Task, StartTime, EndTime)|BeautySchedule]
					) :-
	beautify_conclude_h(Tasks, BeautySchedule).

sort_schedule([], []).
sort_schedule([execs(MT, Tasks)|Schedule], [execs(MT, SortedTasks)|SortedSchedule]) :-
	sort(2, @=<, Tasks, SortedTasks),
	sort_schedule(Schedule, SortedSchedule).

%%%%%%%%%%%%%
% Programme %
%%%%%%%%%%%%%

jobshop_opt(Jobs, Staff, SortedSchedule, Cost, Delta, Timeout) :-
	gather_tasks(Jobs, Tasks),
	make_vars(Tasks, Schedule),										% see constraint (A1)
	calculate_max_time(Tasks, MaxTime),
	fix_time_domain(Schedule, MaxTime),
	sequence_constraint(Schedule, Schedule),						% see constraint (A2)
	disjoint_constraint(Schedule, Schedule),						% see constraint (A3)
	get_all_machine_types(MachineTypes),
	create_time_slots(MachineTypes, MaxTime, TimeSlots, Staff),
	time_slots_constraint(TimeSlots, Schedule),						% see constraint (B1)
	create_time_slices(TimeSlices, MaxTime, Staff),					% see constraint (C1)
	time_slices_constraint(TimeSlices, TimeSlots),					% see constraint (C2)
	define_cost(Schedule, Cost),
	gather_variables(Schedule, TimeSlots, TimeSlices, Vars),
	bb_min(
		search(Vars, 0, input_order, indomain, complete, []),
		Cost,
		bb_options{
			strategy:restart,
			timeout:Timeout,
			delta:Delta
		}
	),
	beautify(Schedule, BeautySchedule),
	sort_schedule(BeautySchedule, SortedSchedule).

make_vars([], _).
make_vars([Task|Tasks], [execs(Task, MachineType, SerialNo, StartTime, EndTime)|Schedule]) :-
	task(Task, MachineType, Duration, _),
	machine(MachineType, N),
	SerialNo #:: [1..N],
	EndTime #= StartTime + Duration,
	make_vars(Tasks, Schedule).

fix_time_domain([], _) :- !.
fix_time_domain([execs(_, _, _, StartTime, EndTime)|Schedule], MaxTime) :-
	StartTime #:: [0..MaxTime],
	EndTime #:: [0..MaxTime],
	fix_time_domain(Schedule, MaxTime).

sequence_constraint([], _).
sequence_constraint([execs(TaskId, _, _, StartTime, _)|Vars], ConstVars) :-
	prev_task(TaskId, PrevTaskId),
	select(execs(PrevTaskId, _, _, _, PrevEndTime), ConstVars, _),
	PrevEndTime #=< StartTime,
	sequence_constraint(Vars, ConstVars).
sequence_constraint([execs(TaskId, _, _, _, _)|Vars], ConstVars) :-
	\+ prev_task(TaskId, _),
	sequence_constraint(Vars, ConstVars).

disjoint_constraint([], _).
disjoint_constraint([Var|Vars], ConstVars) :-
	impose_disjoint_constraint(Var, ConstVars),
	disjoint_constraint(Vars, ConstVars).

impose_disjoint_constraint(_, []).
impose_disjoint_constraint(
							execs(Task1, MachineType, SerialNo1, StartTime1, EndTime1),
							[execs(Task2, MachineType, SerialNo2, StartTime2, EndTime2)|Vars]
						) :-
	Task1 \= Task2,
	(SerialNo1 #= SerialNo2) => ((EndTime1 #=< StartTime2) or (EndTime2 #=< StartTime1)),
	impose_disjoint_constraint(execs(Task1, MachineType, SerialNo1, StartTime1, EndTime1), Vars).
impose_disjoint_constraint(
							execs(Task1, MachineType1, SerialNo1, StartTime1, EndTime1),
							[execs(Task2, MachineType2, _, _, _)|Vars]
						) :-
	 (MachineType1 \= MachineType2 ; Task1 = Task2),
	 impose_disjoint_constraint(execs(Task1, MachineType1, SerialNo1, StartTime1, EndTime1), Vars).


% for every machine type
create_time_slots([], _, [], _) :- !.
create_time_slots([MT|MachineTypes], MaxTime, TimeSlots, Staff) :-
	machine(MT, N),
	create_time_slots(MT, N, MaxTime, TimeSlots1, Staff),
	create_time_slots(MachineTypes, MaxTime, TimeSlots2, Staff),
	append(TimeSlots1, TimeSlots2, TimeSlots).

% for every serial number of this machine
create_time_slots(_, 0, _, [], _).
create_time_slots(MachineType, N, MaxTime, TimeSlots, Staff) :-
	create_time_slots_sec(MachineType, N, MaxTime, TimeSlots1, Staff),
	N > 0,
	M is N-1,
	create_time_slots(MachineType, M, MaxTime, TimeSlots2, Staff),
	append(TimeSlots1, TimeSlots2, TimeSlots).

% for every sec
create_time_slots_sec(_, _, 0, [], _).
create_time_slots_sec(MachineType, SerialNo, Sec, [time_slot(
																MachineType,
																SerialNo,
																Sec,
																OccupiedWorkes
															)|TimeSlots],
															Staff) :-
	OccupiedWorkes #::[0..Staff],
	Sec > 0,
	Sec1 is Sec - 1,
	create_time_slots_sec(MachineType, SerialNo, Sec1, TimeSlots, Staff).

time_slots_constraint([], _).
time_slots_constraint([TimeSlot|TimeSlots], Schedule) :-
	impose_time_slot_constraint(TimeSlot, Schedule),
	time_slots_constraint(TimeSlots, Schedule).

impose_time_slot_constraint(_, []).
impose_time_slot_constraint(
								time_slot(MachineType, SerialNo, Sec, OccupiedWorkes),
								[execs(Task, MachineType, SerialNoVar, StartTime, EndTime)|Schedule]
							) :-
	task(Task, _, _, Workers),
	(
		(SerialNoVar #= SerialNo) 
		and
		(StartTime #< Sec) and (Sec #=< EndTime)
	)
	=>	OccupiedWorkes #= Workers,
	impose_time_slot_constraint(time_slot(MachineType, SerialNo, Sec, OccupiedWorkes), Schedule).
impose_time_slot_constraint(
								time_slot(MachineType1, SerialNo, Sec, OccupiedWorkes),
								[execs(_, MachineType2, _, _, _)|Schedule]
							) :-
	MachineType1 \= MachineType2,
	impose_time_slot_constraint(time_slot(MachineType1, SerialNo, Sec, OccupiedWorkes), Schedule).

create_time_slices([], 0, _).
create_time_slices([time_slice(Sec, OccupiedWorkes)|TimeSlices], Sec, Staff) :-
	OccupiedWorkes #::[0..Staff],
	Sec > 0,
	Sec1 is Sec - 1,
	create_time_slices(TimeSlices, Sec1, Staff).

time_slices_constraint([], _).
time_slices_constraint([time_slice(Sec, OccupiedWorkes)|TimeSlices], TimeSlots) :-
	gather_worker_by_sec(Sec, TimeSlots, SecTimeSlots),
	OccupiedWorkes #= sum(SecTimeSlots),
	time_slices_constraint(TimeSlices, TimeSlots).
	
define_cost(Schedule, Cost) :-
	gather_endtime(Schedule, EndTimes),
	Cost #= max(EndTimes).
