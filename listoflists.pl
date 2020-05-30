%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This code has been tested in ECLiPSe Prolog Version 7.0
%
% Author: Merkouris Papamichail
% email: sdi1400148@di.uoa.gr
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%										Helper
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%
% Number Predicates %
%%%%%%%%%%%%%%%%%%%%%

odd(1) :- !.
odd(N) :- 
	N >= 3,
	M is N - 2,
	odd(M).

even(2) :- !.
even(N) :-
	N >= 2,
	M is N - 1,
	odd(M).

%%%%%%%%%%%%%%%%%%%%
% Lists Operations %
%%%%%%%%%%%%%%%%%%%%

remove_nth_elem([_|Ls], 1, Ls) :- !.
remove_nth_elem([L|Ls], N, [L|Rs]) :-
	M is N-1,
	remove_nth_elem(Ls, M, Rs).

get_nth_elem([L|_], 1, L) :- !.
get_nth_elem([_|Ls], N, R) :-
	M is N - 1,
	get_nth_elem(Ls, M, R).

last([X], [], X) :- !.
last([X|Xs], [X| Init], Last) :-
	last(Xs, Init, Last).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Lists Numerical Operations %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% L = [l1, l2, l3], K = [k1, k2, k3] --> [l1*k1, l2*k2, l3*k3]
list_product([], [], []).
list_product([V|Vs], [U|Us], [VU| VUs]) :-
	VU is V * U,
	list_product(Vs, Us, VUs).

list_sum([], 0).
list_sum([L|Ls], Sum) :-
	list_sum(Ls, Sum1),
	Sum is Sum1 + L.

% !!not to be confused with list product above!!
list_mult(L, P) :-
	list_mult(L, 1, P).

list_mult([], Part, Part).
list_mult([L|Ls], Part, P) :-
	Part1 is Part * L,
	list_mult(Ls, Part1, P).

% applying recursively list_mult/3 to a list of lists, 
% resulting to a list with the coresponding products
list_list_mult([], []).
list_list_mult([L|Ls] , [P|Ps]) :-
	list_mult(L, P),
	list_list_mult(Ls, Ps).

%%%%%%%%%%%%%%%%%%%
% Singleton Lists %
%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% singleton(L, S)
% Given a list L it returns to S a "singleton list",
% as described below:
% Input:	L, a list, L = [l1, l2, l3, .., ln]
% Output:	S = [[l1], [l2], ..., [ln]]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
singleton([], []).
singleton([L|Ls], [[L]|Ps]) :-
	singleton(Ls, Ps).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% elements2singletons(L, S)
% Given a list of lists Ls it returns to Ss a list of
% singleton lists, as described below:
% Input:	Ls, a list of lists, 
%				Ls = [
%						[l1, l2, .., ln1],
%						[k1, k2, .., kn2],
%						..
%						[z1, z2, .., znm]
%					]
% Output:	Ss = [
%						[[l1], [l2], .., [ln1]],
%						[[k1], [k2], .., [kn2]],
%						..
%						[[z1], [z2], .., [znm]]
%					]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
elements2singletons([], []).
elements2singletons([L|Ls], [S|Ss]) :-
	singleton(L, S),
	elements2singletons(Ls, Ss).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% empty_list(L)
% Checking if a list is a list of empty lists, i.e.
%	L = [[], [], ..., []]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
empty_list([]).
empty_list([[]|Ls]) :-
	empty_list(Ls).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Basic Matrix Manipulation %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% first_column(Mat, Col, Rest)
% Input:	Mat, a matrix, in form of list of lists
% Output:	Col, the first column of Mat
%			Rest, the rest of the Mat
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sq_mat_size([Row|_], N) :-
	length(Row, N).

first_column([], [], []).
first_column([[R|Row]|Mat], [R | Col], [Row | Rest]) :-
	first_column(Mat, Col, Rest).

% removing nth column is equivalent with removing
% nth row of the transpose matrix
delete_column(Mat, J, SubMat) :-
	matr_transp(Mat, Mat_t),
	remove_nth_elem(Mat_t, J, SubMat_t),
	matr_transp(SubMat_t, SubMat).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%										Cartesian Product
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% partial_prod(S, Ls, Ps)
% Given a singleton S and a *singleton list* Ls, it
% returns to Ps the cartesian product of {S}xL, as
% described below:
% Input:	S, a singleton S = [s1]
%			Ls, a set let
% 				Ls = [[l1], [l2], [l3], .. [ln]]
% Output:	Ps = [[s1, l1], [s1, l2], ..., [s1, ln]]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
partial_prod(_, [], []).
partial_prod(S, [L|Ls], [SxL | Ps]) :-
	append(S, L, SxL),
	partial_prod(S, Ls, Ps).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% bin_cart_prod(S, T, SxT),
% Given two sets, as *singleton lists* S, T, it returns
% to SxT their cartesian product, as *list*
% Input:	S, a set
%			T, another set
% Output:	SxT the cartesian product of the sets
%			above
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
bin_cart_prod([], _, []).
bin_cart_prod([S|Ss], Ts, P) :-
	partial_prod(S, Ts, P1),
	bin_cart_prod(Ss, Ts, P2),
	append(P1, P2, P).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% cart_prod1(L, P),
% Given a list of *singleton lists* it returns to P
% their cartesian product as a list of lists
% Input:	L, a list of *singleton lists*
% Output:	P, the cartesian product of the given
%				singleton lists, as list of lists
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cart_prod1([P], P) :- !.
cart_prod1([A, B| Ss], P) :-	% S x T x Y = (S x T) x Y
	bin_cart_prod(A, B, P1),
	cart_prod1([P1 | Ss], P).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% cart_prod(L, P)
% Firstly, convert the list of lists L to a singleton
% list Ls, then just use cart_prod1 to get the
% requested result. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cart_prod(L, P) :-
	elements2singletons(L, Ls),
	cart_prod1(Ls, P).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%										Transpose Matrix
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% matr_transp(M, T)
% Input:	M, a matrix, in form of list of lists
% Output:	T, T = M^t
%
% It also works with M as output and T as input
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
matr_transp(EmptyList, []) :-
	empty_list(EmptyList), !.	% given a matrix there is only one transpose (there can be ONLY ONE!!)
matr_transp(Mat, [C | Mat_t]) :-
	first_column(Mat, C, Rest),
	matr_transp(Rest, Mat_t).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%										Matrix Product
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



inner_product(V, U, P) :-
	list_product(V, U, VUs),
	list_sum(VUs, P).

% multiplies a row vecrtos with all the column vectors
% of a matrix
mult_row(_, EmptyList, []) :-
	empty_list(EmptyList), !.
mult_row(Row, Mat, [M | MultRow]) :-
	first_column(Mat, C, Rest),
	inner_product(Row, C, M),
	mult_row(Row, Rest, MultRow).

matr_mult([], _, []).
matr_mult([L|Ls], R, [LxR|LxRs]) :-
	mult_row(L, R, LxR),
	matr_mult(Ls, R, LxRs).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%										Determinant
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The implemantation bellow follows the Laplace Formula for the determinant
% see here [https://en.wikipedia.org/wiki/Determinant#Laplace's_formula_and_the_adjugate_matrix]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% submatrix(Rs, D)
% Input:	M, a matrix, in form of list of lists. Let
%				M = [
%						[1, 2, 3],
%						[4, 5, 6],
%						[7, 8, 9]
%					]
% Output:	D, a list of pairs, e.g.:
%				D = [
%						[1, [[5, 6], [8, 9]]],
%						[-2, [[4, 6], [7, 9]]],
%						[3, [[4, 5], [7, 8]]]
%					]
%
% We will call D a SubMatrix Structure.
%
% In the special case where the M is the singleton
% matrix, e.g. M = [[1]], then D = [[1]]. This detail will help
% us in the predicate resolve_step/2 bellow.
%
% submatrix/2 uses the predicate submatrix1/4
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
submatrix([[X]], [[X]]) :- !.
submatrix([Row|Rows], D) :-
	submatrix1(Row, Rows, 1, D). 

submatrix1([], _, _, []).
submatrix1([R|Row], Mat, N, [D|DetInit]) :-
	delete_column(Mat, N, SubMat),
	odd(N),!,
	D = [R, SubMat],
	M is N + 1,
	submatrix1(Row, Mat, M, DetInit).
submatrix1([R|Row], Mat, N, [D|DetInit]) :-
	delete_column(Mat, N, SubMat),
	even(N),!,
	RMin is -1 * R,
	D = [RMin, SubMat],
	M is N + 1,
	submatrix1(Row, Mat, M, DetInit).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% resolve_step(D, R)
% Input:	D, a submatrix structure, e.g.:
%				D = [
%						[1, [[5, 6], [8, 9]]],
%						[-2, [[4, 6], [7, 9]]],
%						[3, [[4, 5], [7, 8]]]
%					]
% Output:	R, a submatrix structure with even smaller matricies e.g.:
%				R = [
%						[1, 5, [[9]]],
%						[1, -6, [[8]]],
%						[-2, 4, [[9]]],
%						[-2, -6, [[7]]],
%						[3, 4, [[8]]],
%						[3, -5, [[7]]]
%					]
%
% This is the main step for computing the determinant. Our
% goal is to take advantage of the spacial case of
% submatrix/3, and in the end to get just a list of lists.
% Then the determinant is just the product sum of the list
% of lists.
%
% E.g. After one more call of the resolve step in the
% example above we get:
% R = [
%		[1, 5, 9],
%		[1, -6, 8],
%		[-2, 4, 9],
%		[-2, -6, 7],
%		[3, 4, 8],
%		[3, -5, 7]
%	]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
resolve_step([], []).
resolve_step([D|Ds], Out1) :-
	last(D, Init, SubMat),
	submatrix(SubMat, SubSubMat),
	partial_prod(Init, SubSubMat, P),	% we use here partial_prod/3 a bit different than we used it
	resolve_step(Ds, Out2),				% in bin_cart_prod/3
	append(P, Out2, Out1).

resolve(A, 1, A) :- !.
resolve(A, N, C) :-
	M is N - 1,
	resolve_step(A, B),
	resolve(B, M, C).

matr_det(Mat, Det) :-
	submatrix(Mat, SubMats),
	sq_mat_size(Mat, N),
	resolve(SubMats, N, Ds),
	list_list_mult(Ds, Es),
	list_sum(Es, Det).
