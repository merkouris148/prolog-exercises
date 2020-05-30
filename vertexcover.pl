%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This code has been tested in ECLiPSe Prolog Version 7.0
%
% Author: Merkouris Papamichail
% email: sdi1400148@di.uoa.gr
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- lib(ic).
:- lib(branch_and_bound).
:- lib(listut).
:- compile(graph).


vertexcover(N, D, Cover) :-
    create_graph(N, D, Edges),
    writeln("The graph generated is"),
    write("G = "), writeln(Edges),
    length(VarNodes, N),
    node_constrains(VarNodes),
    edge_constrains(VarNodes, Edges),
    Cost #= sum(VarNodes),
    bb_min(
    	search(VarNodes, 0, occurrence, indomain, complete, []),
    	Cost,
    	bb_options{strategy:restart}
    ),
    findall(
    	Ind,
    	nth1(Ind, VarNodes, 1),
    	Cover
    ).

node_constrains([]).
node_constrains([Var|VarNodes]) :-
	Var #:: [0,1],
	node_constrains(VarNodes).

edge_constrains(_, []).
edge_constrains(VarNodes, [E1 - E2|Edges]) :-
	nth1(E1, VarNodes, V1),
	nth1(E2, VarNodes, V2),
	V1 + V2 #>= 1,
	edge_constrains(VarNodes, Edges).
