%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This code has been tested in ECLiPSe Prolog Version 7.0
%
% This programme uses the *fd* library.
%
% Time: finding 1st solution given the example data in
% 		0.02s cpu
%
% Author: Merkouris Papamichail
% email: sdi1400148@di.uoa.gr
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%
%% READ ME %%
%%%%%%%%%%%%%
/********************************************************************
 * Σχετικά με τις παραδοχές που έχουν γίνει στην ΕΙΣΟΔΟ δεδομένων
 *
 * Στην εργασία *δεν* γίνεται η παραδοχή ότι σε κάθε άνθρωπο του
 * αρέσουν όλα τα πρόσωπα του αντίθετου φίλου. Ειδικότερα, στα
 * γεγονότα prefers/2 αν για παράδειγρμα έχουμε:
 * 		prefers(Woman, PreferableMen).
 * μπορεί το PreferableMen να είναι γνήσιο υποσύνολο του Men.
 *
 * Έτσι το πρόγραμμα μπορεί και διαχειρίζεται μια μεγαλύτερη
 * οικογένεια προβλημάτων. 
 ********************************************************************/

:- lib(fd).
% :- compile(stablefd_data).	% to compile the example date uncomment THIS line
:- lib(listut).
:- set_flag(print_depth,1000).

%%%%%%%%%%%%%
% Utilities %
%%%%%%%%%%%%%

man(Person) :-
	men(Men),
	member(Person, Men).

woman(Person) :-
	women(Women),
	member(Person, Women).

people(People) :-
	men(Men),
	women(Women),
	append(Men, Women, People).

get_vars([], []).
get_vars([match(_, VarPartner, VarValue)|Vars], [VarPartner, VarValue|SimpleVars]) :-
	get_vars(Vars, SimpleVars).

beautify([], []).
beautify([match(Person, Partner, _)|Vars], [Person-Partner|Match]) :-
	listut:delete(Vars, match(Partner, Person, _), Vars2),
	beautify(Vars2, Match).

filter_vars(_, [], []).
filter_vars(ConstVars, [Person|People], [match(Person, VarPartner, VarValue)|Vars]) :-
	select(match(Person, VarPartner, VarValue), ConstVars, _),
	filter_vars(ConstVars, People, Vars).

filter_partner(ConstMatch, Person, Partner) :-
	select(match(Person, Partner, _), ConstMatch, _).

prefix([Del|_], Del, []).
prefix([L|List], Del, [L| Pref]) :-
	L \= Del,
	prefix(List, Del, Pref).


better(P1, P2, Better) :-
	prefers(P1, Pref),
	prefix(Pref, P2, Better).

match_partner(ConstMatch, Person, Partner) :-
	woman(Person),
	select(Partner-Person, ConstMatch, _).
match_partner(ConstMatch, Person, Partner) :-
	man(Person),
	select(Person-Partner, ConstMatch, _).

% better_than_partner/3
% Use: better_than_partner(+BetterPartners, +Person, +ConstMatch)
% w.l.g let Person be a man. better_than_partner/3 is true if
% there is a Woman that the Person likes more than his wife
% AND the Woman likes the Person more than her husband
better_than_partner([BetterPartner|_], Person, ConstMatch) :-			% checking every possible better partner for the person
	match_partner(ConstMatch, BetterPartner, CurrentPartner),			% get better partners current match
	better(BetterPartner, CurrentPartner, BetterThanCurrentPartner),	% find the people that better partner prefers more than his/her curresnt partner
	member(Person, BetterThanCurrentPartner).							% check if the better partner also prefers the person more than her/his current partner
better_than_partner([BetterPartner|BPs], Person, ConstMatch) :-
	match_partner(ConstMatch, BetterPartner, CurrentPartner),
	better(BetterPartner, CurrentPartner, BetterThanCurrentPartner),
	\+ member(Person, BetterThanCurrentPartner),						% if this better partner doesn't prefer the person more than his/her current partner
	better_than_partner(BPs, Person, ConstMatch).						% check the rest of the list of better partners

%%%%%%%%%%%%%
% Programme %
%%%%%%%%%%%%%

stable(Match) :-
	make_vars(Vars),
	marriage_constraint(Vars, Vars),
	stability_constraint(Vars, Vars),
	get_vars(Vars, SimpleVars),
	labeling(SimpleVars),
	beautify(Vars, Match).

/*
 * About validation
 * It's easy to see if the marriage constraint is satisfied
 * regarding the beautify/2. If marriage constraint isn't
 * satisfied then |Match| > |Men| = |Women|.
 *
 * For stability constraint see the validator bellow.
 */

marriage_constraint(_, []).
marriage_constraint(ConstVars, [VarMatch|Vars]) :-
	match(Person, _, _) = VarMatch,
	prefers(Person, Prefers),
	impose_marriage(ConstVars, VarMatch, Prefers),
	marriage_constraint(ConstVars, Vars).  

impose_marriage(_, _, []).
impose_marriage(ConstVars, match(Person1, VarPartner1, VarValue1), [PossiblePartner|Prefers]) :-
	filter_partner(ConstVars, PossiblePartner, VarPartner2),
	(VarPartner1 #= PossiblePartner) #=> (VarPartner2 #= Person1),		% marriage constraint	 	% If rick marries jane, jane should also marry rick.
	impose_marriage(ConstVars, match(Person1, VarPartner1, VarValue1), Prefers).					% Marriage is a symmetric relation

make_vars(Vars) :-
	people(People),
	make_vars(People, Vars).

make_vars([], []).
make_vars([Person|People], [match(Person, VarPartner, VarValue) | Vars]) :-
	prefers(Person, Prefers),
	element(VarValue, Prefers, VarPartner),		% determine the domain of the variables				% VarValue is the index of VarPartner in Person's preference
	make_vars(People, Vars).

stability_constraint(_, []).
stability_constraint(ConstVars, [VarMatch|Vars]) :-		% impose stability constraint for every variable
	match(Person, _, _) = VarMatch,
	prefers(Person, Prefers),
	stability_constraint(ConstVars, VarMatch, Prefers),
	stability_constraint(ConstVars, Vars).

stability_constraint(_, _, []).
stability_constraint(ConstVars, VarMatch, [PossiblePartner|Prefers]) :-		% for a fixed variable impose stability constraint for every possible partner
	match(Person, _, _) = VarMatch,
	better(Person, PossiblePartner, BetterThanPartner),
	filter_vars(ConstVars, BetterThanPartner, BetterThanPartnerVars),
	impose_stability(VarMatch, PossiblePartner, BetterThanPartnerVars),
	stability_constraint(ConstVars, VarMatch, Prefers).

impose_stability(_, _, []).
impose_stability(match(Person1, VarPartner1, VarValue1), PossiblePartner, [match(Person2, _, VarValue2)|BetterThanPartnerVars]) :-		% for a fixed pair of a variable and a possible partner
	prefers(Person2, Prefers2),																											% impose stability constraint for every Person's2 partner
	nth1(Value, Prefers2, Person1),																										% that Person1 likes more than (his/her) PossiblePartner
	(VarPartner1 #= PossiblePartner) #=> (VarValue2 #< Value),			% stability constraint			% w.l.g. let Person1 be a woman. Given the husband of Person1, then
	impose_stability(match(Person1, VarPartner1, VarValue1), PossiblePartner, BetterThanPartnerVars).	% all the men that Person1 prefers more than her husband should
																										% like her less than their wife
																										% OR equivalently
																										% the other men should choose a wife a woman that they like more than
																										% Person1.
%%%%%%%%%%%%%
% Validator %
%%%%%%%%%%%%%

stability_validator(Match) :-
	stability_validator(Match, Match).

stability_validator([], _).
stability_validator([M-W|Match], ConstMatch) :-
	better(M, W, Better_Wifes),
	\+ better_than_partner(Better_Wifes, M, ConstMatch),
	better(W, M, Better_Husbands),
	\+ better_than_partner(Better_Husbands, W, ConstMatch),
	stability_validator(Match, ConstMatch).
