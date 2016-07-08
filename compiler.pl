
%	Autor: Wojciech Balik
%	Nr Indeksu: 280254
%
%	(Parser i Lekser sa dosc mocno wzorowane na programie while_parser umieszczonym na kno)
%
%	Kompilator obsługuje wszystkie wlasnosci jezyka oprocz przekazywania parametrow przez nazwe.Dla wygody przyjalem ze
%	gdy przekazujemy parament do procedury to niezaleznie od tego czy wpiszemy slowo "value" czy tez nie, parament zostanie
%	przekazany przez wartosc.
%
%
%
%	Struktura pamieci programu:
%
%  											 
%	 _______________________________________________________
%   |                                                   |   |
%   |    .text 	                            Stos  <---  |   |
%   |___________________________________________________|___|
%	^													  ^
%  	|												      |
%  0x0000										        0xfffe - Stack Pointer
%
%	Struktura ramki stosu:
%	 ____________________________________________________________
%	|local|     |local| arg |     | arg | adres ramki | adres   |
%	|  n  | ... |  1  |  1  | ... |  n	|   rodzica   | powrotu |
%	|_____|_____|_____|_____|_____|_____|_____________|_________|
%	^											  
%	|											  
%  %stack pointer								
%
%	Konwencja wywolania procedury:
%	Strona wywolujaca wrzuca na stos adres powrotu, adres ramki rodzica(*1)
%	Strona wywolywana jest odpowiedzialna za wrzucenie zmiennych lokalnych na stos
%	Za oczyszczenie stosu przed skokiem odpowiedzialna jest funkcja wywolywana
%	Wartosc zwracana przekazywana jest przez rejestr DATA
%
%	(*1) przez adres ramki rodzica nalezy rozumiec najblizsza szczytowi stosu ramke funkcji o zagniezdzeniu n - 1, gdzie n to stopien
%	zagniezdzenia funkcji wywolywanej


:- op(910, xfy, ';;').


algolLexer(Tokens) -->
	czr,
	(  
		(	
			"<>", !, {Token = tokNotEqual};
			"+", !,{Token = tokPlus};
			"-", !, {Token = tokMinus};
			"*", !, {Token = tokMult};
			"<=", !, {Token = tokLesserEqual};
			">=", !, {Token = tokGreaterEqual};
			"<", !, {Token = tokLesser};
			">", !, {Token = tokGreater};
			"=", !, {Token = tokEqual};
			":=", !, {Token = tokAssign};
			";", !, {Token = tokSemicolon};
			",", !, {Token = tokComma};
			"(", !, {Token = tokLeftParen};
			")", !, {Token = tokRightParen};
			% # identyfikatory #
			(litera(L),	
			!,
			identyfikator(L, Id),
			{
				((member( (Id, Token ),
				[
					(and, tokAnd),
					(begin, tokBegin),
					(call, tokCall),
					(div, tokDiv),
					(done, tokDone),
					(do, tokDo),
					(else, tokElse),
					(end, tokEnd),
					(fi, tokFi),
					(if, tokIf),
					(local, tokLocal),
					(mod, tokMod),
					(not, tokNot),
					(or, tokOr),
					(procedure, tokProcedure),
					(program,tokProgram),
					(read, tokRead),
					(return,tokReturn),
					(then, tokThen),
					(value,tokValue),
					(while,tokWhile),
					(write,tokWrite)
				]),!) ;
				Token = tokIdentifier(Id))
			}) ;
				
			

			% # literaly calkowitoliczbowe #
			(cyfra(D),!,
            liczba(D, N),
            { Token = tokNumber(M),
	            (
	            	(
	            		atom(N),
	            		!,
	            		atom_codes(N,A),
	            		number_codes(M,A)
	            	) ;
	            	(
	            		number_codes(M, N)
	            	)
	            ) 
        	})
      	),!,
      	{Tokens = [Token|TokTail]},
      	algolLexer(TokTail) ;

  		[],
        { Tokens = [] }
    ).


identyfikator(L, I) --> 
	ciag_liter(Z),
	{atom_codes(I, [L|Z])}.

litera(X) --> [X], { code_type(X, alpha) }.

ciag_liter([H|T]) --> [H], {code_type(H, alnum),! ; H = 95,! ; H = 39}, !, ciag_liter(T).
ciag_liter([]) --> [].


liczba(C, L) --> 
	ciag_liter(R),
	{atom_chars(L, [C|R])}.

cyfra(X) --> {code_type(X, digit)}, [X].

ciag_cyfr([H|T]) --> cyfra(H), !, ciag_cyfr(T) | [].


bialy_znak --> {code_type(X, space)}, [X].

komentarz --> "(*", ciag_znakow_kom.

znak_kom --> {between(0, 255, X)}, [X].

ciag_znakow_kom --> "*)",!.

ciag_znakow_kom --> znak_kom, !, ciag_znakow_kom | [].

znak_rozdzielajacy --> komentarz | bialy_znak.

czr --> znak_rozdzielajacy, !, czr | [].



program(P) --> [tokProgram], [tokIdentifier(_)], blok(P).

blok(b(Deklaracje, Instr)) --> deklaracje(Deklaracje), [tokBegin], instrukcja_zlozona(Instr), [tokEnd].

deklaracje(L) -->  deklaracja(H), !, deklaracje(T), {flatten([H|T],L)}.
deklaracje([]) -->  [].

deklaracja(D) --> deklarator(D),!.
deklaracja(D) --> procedura(D).

deklarator(D) --> [tokLocal], zmienne(D).

zmienne([H|T]) --> zmienna(H), [tokComma], !, zmienne(T).
zmienne([H]) --> zmienna(H).

zmienna(var(Id)) --> [tokIdentifier(Id)].

procedura(procedure(Id, Args, BlokInstr)) -->  
	[tokProcedure], [tokIdentifier(Id)], [tokLeftParen], argumenty_formalne(Args), [tokRightParen], blok(BlokInstr).

argumenty_formalne(Args) -->  ciag_arg_formalnych(Args), !.
argumenty_formalne([]) --> [].


ciag_arg_formalnych([H|T]) --> argument_formalny(H), [tokComma], !, ciag_arg_formalnych(T).
ciag_arg_formalnych([H]) --> argument_formalny(H).


argument_formalny([Arg]) --> [tokValue], !, zmienna(Arg).
argument_formalny([Arg]) --> zmienna(Arg).

instrukcja_zlozona(H ';;' T) --> instrukcja(H), [tokSemicolon], !, instrukcja_zlozona(T).
instrukcja_zlozona(H ';;' end) --> instrukcja(H). 




instrukcja(condit_expr(Cond, Instr_If, Instr_Else)) --> 
	[tokIf], wyrazenie_logiczne(Cond), [tokThen], instrukcja_zlozona(Instr_If), [tokElse], !, instrukcja_zlozona(Instr_Else), [tokFi].

instrukcja(condit_expr(Cond, Instr)) --> 
	[tokIf], !, wyrazenie_logiczne(Cond), [tokThen], instrukcja_zlozona(Instr), [tokFi].

instrukcja(while(Cond, Instr)) -->
	[tokWhile], !, wyrazenie_logiczne(Cond), [tokDo], instrukcja_zlozona(Instr), [tokDone].

instrukcja(call(Proc,Args)) -->
	[tokCall], !, wywolanie_procedury(proc_call(Proc, Args)).

instrukcja(return(Val)) --> 
	[tokReturn], !, wyrazenie_arytmetyczne(Val).

instrukcja(read(Var)) -->
	[tokRead], !, zmienna(Var).

instrukcja(write(Val)) -->
	[tokWrite], !, wyrazenie_arytmetyczne(Val).

instrukcja(assign(Var,Value)) --> 
	zmienna(Var), [tokAssign], !, wyrazenie_arytmetyczne(Value).


wyrazenie_arytmetyczne(Expr) -->
	skladnik(S), wyrazenie_arytmetyczne(S, Expr).

wyrazenie_arytmetyczne(S, Expr) -->
	operator_addytywny(Op), !, skladnik(S1),
	{A = [Op, S, S1]}, wyrazenie_arytmetyczne(A, Expr).
wyrazenie_arytmetyczne(A,A) --> [].

operator_addytywny(+) --> [tokPlus].
operator_addytywny(-) --> [tokMinus].

skladnik(S) --> czynnik(C), skladnik(C, S).

skladnik(A, S) --> 
	operator_multiplikatywny(Op), !, czynnik(R),
	{A1 = [Op, A, R]}, skladnik(A1, S).  %%%%%%%%%%%%%%%%%%%%%%%%%%%
skladnik(S, S) --> [].

operator_multiplikatywny(*) -->  [tokMult].
operator_multiplikatywny(div) -->  [tokDiv].
operator_multiplikatywny(mod) -->  [tokMod].

czynnik(-C) --> 
	[tokMinus], !, wyrazenie_proste(C).
czynnik(C) -->
	wyrazenie_proste(C).

wyrazenie_proste(Expr) --> 
	[tokLeftParen], !, wyrazenie_arytmetyczne(Expr), [tokRightParen].
wyrazenie_proste(Expr) -->
	wyrazenie_atomowe(Expr).

wyrazenie_atomowe(Expr) -->	wywolanie_procedury(Expr), !. 
wyrazenie_atomowe(Expr) --> zmienna(Expr), !.
wyrazenie_atomowe(number(N)) --> [tokNumber(N)].

wywolanie_procedury(proc_call(Id, Args)) --> 
	[tokIdentifier(Id)], [tokLeftParen], !, argumenty_faktyczne(Args), [tokRightParen].

argumenty_faktyczne(Args) --> 
	ciag_arg_faktycznych(Args), !.
argumenty_faktyczne([]) --> [].

ciag_arg_faktycznych([H|T]) --> 
	argument_faktyczny(H), [tokComma], !, ciag_arg_faktycznych(T).
ciag_arg_faktycznych([H]) -->
	argument_faktyczny(H).

argument_faktyczny(Arg) --> wyrazenie_arytmetyczne(Arg).

wyrazenie_logiczne([or, P, Q]) --> 
	koniunkcja(P), [tokOr], !, wyrazenie_logiczne(Q).
wyrazenie_logiczne(P) --> 
	koniunkcja(P). 

koniunkcja([and, P, Q]) --> 	
	warunek(P), [tokAnd], koniunkcja(Q).
koniunkcja(P) --> warunek(P).


warunek([not,Cond]) --> 
	[tokNot], !, wyrazenie_relacyjne(Cond).
warunek(Cond) -->
	wyrazenie_relacyjne(Cond). 

wyrazenie_relacyjne(Expr) --> 
	wyrazenie_arytmetyczne(L), operator_relacyjny(Op), !, wyrazenie_arytmetyczne(R),
	{ Expr = [Op, L, R] }. 
wyrazenie_relacyjne(Expr) -->
	[tokLeftParen], wyrazenie_logiczne(Expr), [tokRightParen].

operator_relacyjny(<) --> [tokLesser],!. 
operator_relacyjny(<=) --> [tokLesserEqual],!.
operator_relacyjny(>) --> [tokGreater],!.
operator_relacyjny(>=) --> [tokGreaterEqual],!.
operator_relacyjny(=) --> [tokEqual],!.
operator_relacyjny(<>) --> [tokNotEqual].

parse(L, T) :-
	phrase(algolLexer(TokList), L),
   	phrase(program(T), TokList).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%																																%
%															kompilator															%
%																																%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% 	Predykaty maja dosc duzo argumentow.Wynika to z potrzeby pamietania i przekazywania stanu kompilacji(tzn pamietania
%	jakiej procedury kod wlasnie tlumaczymy, w ktorej procedurze jest ona zadeklarowana itp).


algol16(L, Bytes) :-
	parse(L, Abst),
	FullList = [[const,swapa,const,store],[0xfffe],[0xfffd] | FullList1],
	generate_code(procedure(program, [], Abst) , Code, Procedures, _, [(null, 0, -1, [], [])], 0, _),
	tree_to_list(List,[(Code, Procedures)]), 
	append_all(List, FullList1),
	generate_address_dict(FullList, AddressDict, 0),
	reloc(FullList, AddressDict, RelocList), 
	translation(RelocList,Bytes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% \/ \/ Predykaty sluzace do testow.Byc moze sie przydadza wiec zostawiam 
compile(String, OutFilename) :-
	open(String, read, Stream),
  	read_stream_to_codes(Stream, L),
  	close(Stream),
	algol16(L, Bytes),
	writeBinaryList(Bytes, OutFilename).

writeWords([], _).
 
writeWords([Word | Rest], Fd) :-
    HiByte is Word >> 8,
    LoByte is Word  /\ 0xFF,
    put_byte(Fd, HiByte),
    put_byte(Fd, LoByte),
    writeWords(Rest, Fd).
 
writeBinaryList(List, Filename) :-
    open(Filename, write, Fd, [type(binary)]),
    writeWords(List, Fd),
    close(Fd).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

to_opcodes(X, Y) :-
	member((X,Y),
		[
			(nop,0),
			(syscall,1),
			(load,2),
			(store,3),
			(swapa,4),
			(swapd,5),
			(branchz,6),
			(branchn,7),
			(jump,8),
			(const,9),
			(add,10),
			(sub,11),
			(mul,12),
			(div,13)
		]).

translation([],[]).
translation([H | T],Result) :-
	(
		H = addr(_,_),
		!,
		translation(T, Result)
	) ;
	(
		H = label(_),
		!,
		translation(T, Result)
	) ;
	(
		H = [A1, A2, A3, A4],
		!,
		to_opcodes(A1, B1),
		to_opcodes(A2, B2),
		to_opcodes(A3, B3),
		to_opcodes(A4, B4),
		R is B4 + 16 * B3 + 256 * B2 + 4096 * B1,
		%atom_string(R,R1),

		Result = [R | T1],
		translation(T,T1)
	) ;
	(
		H = [N],

		Result = [N|T1],
		translation(T,T1)
	) ;
	(
		translation(T,Result)
	).

reloc([],_,[]).
reloc([[addr(Id,Nlevel)] | T], Dict, [[Address] | T1]) :-
	member((addr(Id,Nlevel), Address), Dict),
	!,
	reloc(T, Dict, T1).
reloc([[label(N)] | T], Dict, [[Address] | T1]) :-
	member((label(N), Address), Dict),
	!,
	reloc(T, Dict, T1).
reloc([H|T], Dict, [H|T1]) :-
	reloc(T, Dict, T1).



% po wykonaniu predykatu generate_code ,wygenerowane procedury maja strukture drzewiasta, wiec trzeba ja przerobic na liste
tree_to_list([],[]).
tree_to_list([Code | List], [(Code, Procedures) | T]) :-
	add_to_stack(T, Procedures, NewT),
	tree_to_list(List, NewT).

add_to_stack(Stack, [], Stack).
add_to_stack(Stack, [H | T], [(Proc_Code, Proc_Local_Procedures) | NewStack]) :-
	H = (_, _, _, Proc_Code, _, Proc_Local_Procedures),
	add_to_stack(Stack, T, NewStack).

append_all([],[]).
append_all([[] | T1], T2) :-
	append_all(T1, T2).
append_all([ [H|T1] | T2 ], [H | T3]) :-
	!,
	append_all([T1 | T2], T3).
append_all([H | T1], [H | T2]) :-
	append_all(T1, T2).

generate_address_dict([],[],_).
generate_address_dict([addr(Id, Nlevel) | T1], [(addr(Id, Nlevel), PC) | T2], PC) :-
	!,
	generate_address_dict(T1, T2, PC).
generate_address_dict([label(N) | T1], [(label(N), PC) | T2], PC) :-
	!,
	generate_address_dict(T1, T2, PC).
generate_address_dict([[_,_,_,_] | T1], T2, PC) :-
	!,
	PC1 is PC + 1,
	generate_address_dict(T1, T2, PC1).
generate_address_dict([[_] | T1], T2, PC) :-
	!,
	PC1 is PC + 1,
	generate_address_dict(T1, T2, PC1).
generate_address_dict([_ | T1], T2, PC) :-
	generate_address_dict(T1, T2, PC).


% 	Procedures = [ (ProcId, Nesting_Level, Proc_Code, Proc_Local_Vars, Proc_Local_Procedures) | T ] 
%		Proc_Local_Vars = [(Id, Offset) | T]
%	Locals = [(Id, Offset) | T]
%				
%	CurrentState = (Id, Frame_Size, Nesting_Level, Local_Vars, Local_Procedures)

%generate_code(+Procedure, -Code, -Procedures, -Locals, +Parent, +Label_Counter, -Label_Counter_Res )
%Locals - slownik(w formie listy) w ktorym klucze to identyfikatory zmiennych lokalnych i argumentow a wartosci to offsety wzgledem ramki stosu
generate_code(procedure(Id, Args, b(Declarations, Instructions)), Code, Procedures, Locals, [Parent | T], Label_Counter, Label_Counter_Res) :-
	get_frame_size(Frame_Size, Args, Declarations),
	get_locals_size(Declarations, 0 ,Offset),
	Parent = (_, _, PNesting_Level, _, _),
	Nlevel is PNesting_Level + 1,

	manage_declarations(Declarations, Procedures, Locals, [Parent | T], (Id, Frame_Size, Nlevel, [], []), 0, Label_Counter, Label_Counter1),
	manage_arguments(Args, Offset, Arg_List, _), 
	
	Code1 = [addr(Id, Nlevel),[const, swapa, load, swapd], [0xfffe], [const, swapd, sub, swapa], [Offset], [const, swapa, store, nop],[0xfffe] | Tail], % Stack_Pointer -= Offset
	generate_actual_code(Instructions, Tail, Procedures, Locals, Arg_List, [Parent | T], (Id, Frame_Size, Nlevel), Label_Counter1, Label_Counter_Res),
	(
		(
			Parent = (null, 0, -1, [], []),
			!,
			Code2 = [[const,syscall,nop,nop],[0]]
		) ;
		(
			Off is Frame_Size + 2,
			Code2 = [[swapd,const,swapa,load],[0xfffe],[swapd,swapa,const,add],[Off],[swapa,swapd,const,swapa],[0xfffe],
					 [store,swapd,swapa,const],[1],[swapd,sub,swapa,swapd],[load,jump,nop,nop]]
		)
	),
	append_all([Code1,Code2], Code).




get_locals_size([],X,X).
get_locals_size([var(_) | T], N, X) :-
	!,
	N1 is N + 1,
	get_locals_size(T, N1 , X).

get_locals_size([_ | T], N, X) :-
	get_locals_size(T, N, X).

get_frame_size(N, Args, Declarations) :-
	get_locals_size(Declarations, 0, DSize),
	length(Args, ASize),
	N is DSize + ASize.







%manage_declarations(+Declarations, -Procedures, -Local_Vars, +Parent, +Current_State, +Offset, +Label_Counter, -Label_Counter_Res)

manage_declarations([], [], [], _, _, _, Label_Counter, Label_Counter) :- 
	!.

manage_declarations([var(Id) | Declarations], Procedures, [(Id, Offset) | Local_Vars],Parent, Current_State, Offset, Label_Counter, Label_Counter_Res) :-
	!,
	Current_State = (CId, CFrame_Size, CNesting_Level, CLocal_Vars, CLocal_Procedures),
	Next_State = (CId, CFrame_Size, CNesting_Level, [(Id, Offset) | CLocal_Vars], CLocal_Procedures),
	Offset1 is Offset + 1,
	manage_declarations(Declarations, Procedures, Local_Vars, Parent, Next_State, Offset1, Label_Counter, Label_Counter_Res).


manage_declarations([procedure(Id, Args, Instructions) | Declarations], [ H | Procedures], Local_Vars, Parent, Current_State, Offset, Label_Counter, Label_Counter_Res) :-
	Instructions = b(Decl, _),
	Current_State = (CId, CFrame_Size, CNesting_Level, CLocal_Vars, CLocal_Procedures),
	generate_code(procedure(Id, Args, Instructions), Proc_Code, Proc_Local_Procedures, Proc_Local_Vars, [ Current_State | Parent], Label_Counter, Label_Counter1),
	get_frame_size(PFrame_Size, Args, Decl),
	Nlevel is CNesting_Level + 1,
	H = (Id, addr(Id,Nlevel), PFrame_Size, Proc_Code, Proc_Local_Vars, Proc_Local_Procedures), 
	Next_State = (CId, CFrame_Size, CNesting_Level, CLocal_Vars, [H|CLocal_Procedures]),
	manage_declarations(Declarations, Procedures, Local_Vars, Parent, Next_State, Offset, Label_Counter1, Label_Counter_Res).

manage_arguments([],Offset, [], Offset).

manage_arguments([var(Id) | T], Offset, [(Id, OffsetRes1) | ArgsRes], OffsetRes) :-
	manage_arguments(T, Offset, ArgsRes, OffsetRes1),
	OffsetRes is OffsetRes1 + 1.

manage_arguments([[var(Id)] | T], Offset, [(Id, OffsetRes1) | ArgsRes], OffsetRes) :-
	manage_arguments(T, Offset, ArgsRes, OffsetRes1),
	OffsetRes is OffsetRes1 + 1.


% ||
% || Predykat odpowiada za tlumaczenie instrukcji algola16 na instrukcje asemblera.Nazwa predykatu moze wydawac sie dziwna
% || ale z przyzwyczajenia ja zostawiam 
% \/
generate_actual_code(end, [], _, _, _, _, _, Label_Counter, Label_Counter).

generate_actual_code(Instr ';;' Rest, Result, Procedures, Locals, Arg_List, Parent, CurrentState, Label_Counter, Label_Counter_Res) :-
	(
		Instr = assign(var(Id), ArithmExpression),
		!,
		Result1 = [assign | Result11],
		compute_value(ArithmExpression, Result11, Procedures, Locals, Arg_List, Parent, CurrentState, 0, Label_Counter, Label_Counter1),  % ACC = Value(Expression)
		push(Result2),
		find_variable_address(var(Id), Result3, Locals, Arg_List, Parent, CurrentState, 1),
		pop(Result4),
		Result5 = [[swapa,swapd,swapa,store],assign_end],
		generate_actual_code(Rest, X, Procedures, Locals, Arg_List, Parent, CurrentState, Label_Counter1, Label_Counter_Res),
		append_all([Result1, Result2, Result3, Result4, Result5, X], Result)
	) ;
	(
		Instr = read(var(Id)),
		!,
		find_variable_address(var(Id), Result1, Locals, Arg_List, Parent, CurrentState, 0),
		Result2 = [[swapa,const,syscall,store],[1]],
		generate_actual_code(Rest, Result3, Procedures, Locals, Arg_List, Parent, CurrentState, Label_Counter, Label_Counter_Res),
		append_all([Result1,Result2,Result3], Result)
	) ;
	(
		Instr = write(Expression),
		!,
		compute_value(Expression, Result1, Procedures, Locals, Arg_List, Parent, CurrentState, 0, Label_Counter, Label_Counter1),
		Result2 = [[swapd,const,syscall,nop],[2]],
		generate_actual_code(Rest, Result3, Procedures, Locals, Arg_List, Parent, CurrentState, Label_Counter1, Label_Counter_Res),
		append_all([Result1,Result2,Result3], Result)
	) ;
	(
		CurrentState = (_, AcFrame_Size, _),
		Instr = return(Expression),
		!,
		Off is AcFrame_Size + 2,
		compute_value(Expression, Result1, Procedures, Locals, Arg_List, Parent, CurrentState, 0, Label_Counter, Label_Counter_Res),
		Result2 = [[swapd,const,swapa,load],[0xfffe],[swapd,swapa,const,add],[Off],[swapa,swapd,const,swapa],[0xfffe],
					 [store,swapd,swapa,const],[1],[swapd,sub,swapa,swapd],[load,jump,nop,nop]],
		append_all([Result1,Result2], Result)
	) ;
	(
		Instr = call(Id, Args),
		!,
		call_procedure(Id, Args, Result1, Procedures, Locals, Arg_List, Parent, CurrentState, 0, Label_Counter, Label_Counter1),
		generate_actual_code(Rest, Result2, Procedures, Locals, Arg_List, Parent, CurrentState, Label_Counter1, Label_Counter_Res),
		append_all([Result1, Result2], Result)
	) ;
	(
		Instr = while(Condition, Instructions),
		!,
		Result1 = [label(Loop_Begin) | Result11],
		Loop_Begin = Label_Counter,
		Label_Counter1 is Label_Counter + 1,
		evaluate_condition(Condition, Result11, Procedures, Locals, Arg_List, Parent, CurrentState, 0, Label_Counter1, Label_Counter2),
		Result2 = [[swapa,const,swapa,branchz],[label(Loop_Exit)] | Result21],
		Loop_Exit = Label_Counter2,
		Label_Counter3 is Label_Counter2 + 1,
		generate_actual_code(Instructions, Result21, Procedures, Locals, Arg_List, Parent, CurrentState, Label_Counter3, Label_Counter4),
		Result3 = [[const,jump,nop,nop],[label(Loop_Begin)],label(Loop_Exit) | Result31],
		generate_actual_code(Rest, Result31, Procedures, Locals, Arg_List, Parent, CurrentState, Label_Counter4, Label_Counter_Res),
		append_all([Result1, Result2, Result3], Result)
	) ;
	(
		Instr = condit_expr(Condition, Instructions),
		!,
		evaluate_condition(Condition, Result1, Procedures, Locals, Arg_List, Parent, CurrentState, 0, Label_Counter, Label_Counter1),
		Result2 = [[swapa,const,swapa,branchz],[label(Not_Entering_If)]],
		Label_Counter2 is Label_Counter1 + 1,
		Not_Entering_If is Label_Counter1,
		generate_actual_code(Instructions, Result3, Procedures, Locals, Arg_List, Parent, CurrentState, Label_Counter2, Label_Counter3),
		Result4 = [label(Not_Entering_If) | Result41],
		generate_actual_code(Rest, Result41, Procedures, Locals, Arg_List, Parent, CurrentState, Label_Counter3, Label_Counter_Res),
		append_all([Result1, Result2, Result3, Result4],Result)
	) ;
	(
		Instr = condit_expr(Condition, Instr_If, Instr_Else),
		!,
		evaluate_condition(Condition, Result1, Procedures, Locals, Arg_List, Parent, CurrentState, 0, Label_Counter, Label_Counter1),
		Result2 = [[swapa,const,swapa,branchz],[label(Else)]],
		Label_Counter2 is Label_Counter1 + 1,
		Else = Label_Counter1,
		generate_actual_code(Instr_If, Result3, Procedures, Locals, Arg_List, Parent, CurrentState, Label_Counter2, Label_Counter3),
		Result4 = [[const,jump,nop,nop],[label(Exiting_If)],label(Else) | Result41],
		Exiting_If = Label_Counter3,
		Label_Counter4 is Label_Counter3 + 1,
		generate_actual_code(Instr_Else, Result41, Procedures, Locals, Arg_List, Parent, CurrentState, Label_Counter4, Label_Counter5),
		Result5 = [label(Exiting_If) | Result51],
		generate_actual_code(Rest, Result51, Procedures, Locals, Arg_List, Parent, CurrentState, Label_Counter5, Label_Counter_Res),
		append_all([Result1, Result2, Result3, Result4, Result5],Result)
	).



evaluate_condition(Condition, Result, Procedures, Locals, Arg_List, Parent, CurrentState, On_Stack, Label_Counter, Label_Counter_Res) :-
	(
		Condition = [not,Cond],
		evaluate_condition(Cond, Result1, Procedures, Locals, Arg_List, Parent, CurrentState, On_Stack, Label_Counter, Label_Counter1),
		Result2 = [[swapa,const,swapa,branchz],[label(False)],[const,swapa,const,swapa],[0],[label(Exit)],[branchz,nop,nop,nop],label(False),
				   [const,nop,nop,nop],[1],label(Exit)],
		False is Label_Counter1,
		Exit is Label_Counter1 + 1,
		Label_Counter_Res is Label_Counter1 + 2,
		append_all([Result1, Result2], Result)
	) ;
	(
		Condition = [<, LeftExpr, RightExpr],
		!,
		compute_value(RightExpr, Result1, Procedures, Locals, Arg_List, Parent, CurrentState, On_Stack, Label_Counter, Label_Counter1),
		push(Result2),
		On_Stack1 is On_Stack + 1,
		compute_value(LeftExpr, Result3, Procedures, Locals, Arg_List, Parent, CurrentState, On_Stack1, Label_Counter1, Label_Counter2),
		pop(Result4),
		Result5 = [[swapd,sub,swapa,const], [label(True_Condition)], [swapa,branchn,const,swapa],[label(End)],[const,branchz,nop,nop],[0],
				   label(True_Condition), [const,nop,nop,nop],[1],label(End)],
		True_Condition is Label_Counter2,
		End is Label_Counter2 + 1,
		Label_Counter_Res is Label_Counter2 + 2,
		append_all([Result1, Result2, Result3, Result4, Result5], Result)
	) ;
	(
		Condition = [<=, LeftExpr, RightExpr],
		!,
		compute_value(RightExpr, Result1, Procedures, Locals, Arg_List, Parent, CurrentState, On_Stack, Label_Counter, Label_Counter1),
		push(Result2),
		On_Stack1 is On_Stack + 1,
		compute_value(LeftExpr, Result3, Procedures, Locals, Arg_List, Parent, CurrentState, On_Stack1, Label_Counter1, Label_Counter2),
		pop(Result4),
		Result5 = [[swapd,sub,swapa,const], [label(True_Condition)], [swapa,branchn,branchz,const],[label(End)],[swapa,const,branchz,nop],[0],
				   label(True_Condition), [const,nop,nop,nop],[1],label(End)],
		True_Condition is Label_Counter2,
		End is Label_Counter2 + 1,
		Label_Counter_Res is Label_Counter2 + 2,
		append_all([Result1, Result2, Result3, Result4, Result5], Result)
	) ;
	(
		Condition = [>, LeftExpr, RightExpr],
		!,
		compute_value(LeftExpr, Result1, Procedures, Locals, Arg_List, Parent, CurrentState, On_Stack, Label_Counter, Label_Counter1),
		push(Result2),
		On_Stack1 is On_Stack + 1,
		compute_value(RightExpr, Result3, Procedures, Locals, Arg_List, Parent, CurrentState, On_Stack1, Label_Counter1, Label_Counter2),
		pop(Result4),
		Result5 = [[swapd,sub,swapa,const], [label(True_Condition)], [swapa,branchn,const,swapa],[label(End)],[const,branchz,nop,nop],[0],
				   label(True_Condition), [const,nop,nop,nop],[1],label(End)],
		True_Condition is Label_Counter2,
		End is Label_Counter2 + 1,
		Label_Counter_Res is Label_Counter2 + 2,
		append_all([Result1, Result2, Result3, Result4, Result5], Result)
	) ;
	(
		Condition = [>=, LeftExpr, RightExpr],
		!,
		compute_value(LeftExpr, Result1, Procedures, Locals, Arg_List, Parent, CurrentState, On_Stack, Label_Counter, Label_Counter1),
		push(Result2),
		On_Stack1 is On_Stack + 1,
		compute_value(RightExpr, Result3, Procedures, Locals, Arg_List, Parent, CurrentState, On_Stack1, Label_Counter1, Label_Counter2),
		pop(Result4),
		Result5 = [[swapd,sub,swapa,const], [label(True_Condition)], [swapa,branchn,branchz,const],[label(End)],[swapa,const,branchz,nop],[0],
				   label(True_Condition), [const,nop,nop,nop],[1],label(End)],
		True_Condition is Label_Counter2,
		End is Label_Counter2 + 1,
		Label_Counter_Res is Label_Counter2 + 2,
		append_all([Result1, Result2, Result3, Result4, Result5], Result)
	) ;
	(
		Condition = [=, LeftExpr, RightExpr],
		!,
		compute_value(RightExpr, Result1, Procedures, Locals, Arg_List, Parent, CurrentState, On_Stack, Label_Counter, Label_Counter1),
		push(Result2),
		On_Stack1 is On_Stack + 1,
		compute_value(LeftExpr, Result3, Procedures, Locals, Arg_List, Parent, CurrentState, On_Stack1, Label_Counter1, Label_Counter2),
		pop(Result4),
		Result5 = [[swapd,sub,swapa,const], [label(True_Condition)], [swapa,branchz,const,swapd],[0],[const,swapa,swapd,branchz],[label(End)],
				   label(True_Condition), [const,nop,nop,nop],[1],label(End)],
		True_Condition is Label_Counter2,
		End is Label_Counter2 + 1,
		Label_Counter_Res is Label_Counter2 + 2,
		append_all([Result1, Result2, Result3, Result4, Result5], Result)
	) ;
	(
		Condition = [<>, LeftExpr, RightExpr],
		!,
		compute_value(RightExpr, Result1, Procedures, Locals, Arg_List, Parent, CurrentState, On_Stack, Label_Counter, Label_Counter1),
		push(Result2),
		On_Stack1 is On_Stack + 1,
		compute_value(LeftExpr, Result3, Procedures, Locals, Arg_List, Parent, CurrentState, On_Stack1, Label_Counter1, Label_Counter2),
		pop(Result4),
		Result5 = [[swapd,sub,swapa,const], [label(False_Condition)], [swapa,branchz,const,swapd],[1],[const,jump,nop,nop],[label(End)],
				   label(False_Condition), [const,swapd,nop,nop],[0],label(End),[swapd,nop,nop,nop]],
		False_Condition is Label_Counter2,
		End is Label_Counter2 + 1,
		Label_Counter_Res is Label_Counter2 + 2,
		append_all([Result1, Result2, Result3, Result4, Result5], Result)
	) ;
	(
		Condition = [or, Left_Subtree, Right_Subtree],
		!,
		evaluate_condition(Left_Subtree, Result1, Procedures, Locals, Arg_List, Parent, CurrentState, On_Stack, Label_Counter, Label_Counter1),
		push(Result2),
		On_Stack1 is On_Stack + 1,
		evaluate_condition(Right_Subtree, Result3, Procedures, Locals, Arg_List, Parent, CurrentState, On_Stack1, Label_Counter1, Label_Counter_Res),
		pop(Result4),
		Result5 = [[add,nop,nop,nop]],
		append_all([Result1, Result2, Result3, Result4, Result5],Result)
	) ;
	(
		Condition = [and, Left_Subtree, Right_Subtree],
		evaluate_condition(Left_Subtree, Result1, Procedures, Locals, Arg_List, Parent, CurrentState, On_Stack, Label_Counter, Label_Counter1),
		push(Result2),
		On_Stack1 is On_Stack + 1,
		evaluate_condition(Right_Subtree, Result3, Procedures, Locals, Arg_List, Parent, CurrentState, On_Stack1, Label_Counter1, Label_Counter_Res),
		pop(Result4),
		Result5 = [[mul,nop,nop,nop]],
		append_all([Result1, Result2, Result3, Result4, Result5],Result)
	).




% compute_value to predykat generujacy kod, ktory ma taka wlasnosc ze po jego wykonaniu w akumulatorze znajduje sie 
% wartosc podanego wyrazenia arytmetycznego. Podczas wykonania na stos wrzucane sa zmienne ktore nie sa ani zmiennymi lokalnymi ani
% argumentami, wiec konieczne jest zapamietywanie ile takich pomocniczych zmiennych jest na stosie.Sluzy do tego zmienna On_Stack

compute_value(ArithmExpression, Result, Procedures, Locals, Arg_List, Parent, CurrentState, On_Stack, Label_Counter, Label_Counter_Res) :- 
	(
		ArithmExpression = -Expr,
		!,
		compute_value(Expr, Result1, Procedures, Locals, Arg_List, Parent, CurrentState, On_Stack, Label_Counter, Label_Counter_Res),
		Result2 = [[swapd,const,swapd,mul],[0xffff]],
		append_all([Result1, Result2], Result)
	) ;
	(
		ArithmExpression = number(N),
		!,
		Result = [[const,nop,nop,nop],[N]],
		Label_Counter_Res = Label_Counter
	) ;
	(
		ArithmExpression = var(Id),!,
		find_variable_address(var(Id), Result1, Locals, Arg_List, Parent, CurrentState, On_Stack),
		Result2 = [[swapa,load,nop,nop]],
		append(Result1,Result2, Result),
		Label_Counter_Res = Label_Counter
	) ;
	(
		ArithmExpression = proc_call(Id, Args),
		!,
		call_procedure(Id, Args, Result1, Procedures, Locals, Arg_List, Parent, CurrentState, On_Stack, Label_Counter, Label_Counter_Res),
		Result2 = [[swapd,nop,nop,nop]],
		append_all([Result1, Result2],Result)
	) ;
	(
		ArithmExpression = [Op, Left_Subtree, Right_Subtree], !,
		compute_value(Left_Subtree, Result1, Procedures, Locals, Arg_List, Parent, CurrentState,On_Stack, Label_Counter, Label_Counter1),
		push(Result2),
		On_Stack1 is On_Stack + 1,

		compute_value(Right_Subtree, Result3, Procedures, Locals, Arg_List, Parent, CurrentState, On_Stack1, Label_Counter1, Label_Counter_Res),
		pop(Result4),
		(
			(
				Op = +,
				!,
				Result5 = [[add,nop,nop,nop]]
			) ;
			(
				Op = -,
				!,
				Result5 = [[sub,nop,nop,nop]]
			) ;
			(
				Op = *,
				!,
				Result5 = [[mul,nop,nop,nop]]
			) ;
			(
				Op = div,
				!,
				Result5 = [[div,nop,nop,nop]]
			) ;
			(
				Op = mod,
				!,
				Result5 = [[swapd,swapa,const,add],[0],[swapa,swapd,div,mul],[swapd,swapa,sub,nop]]
			)
		),
		append_all([Result1, Result2, Result3, Result4, Result5], Result)
	).


% predykat generuje kod ktory zajmuje sie stosem i wywoluje dana procedure
call_procedure(Id, Args, Result, Procedures, Locals, Arg_List, Parent, CurrentState, On_Stack, Label_Counter, Label_Counter_Res) :-
	CurrentState = (AcId, AcFrame_Size, AcNlevel),
	length(Args, N),
	Frame_Size is N + 2,
	Result1 = [[const, swapa, load, swapd],[0xfffe], [const, swapd, sub, swapa],[Frame_Size],[const,swapa,store,nop],[0xfffe]], % SP -= Frame_Size
	On_Stack1 is On_Stack + Frame_Size,
	compute_args(Args, Result2, Procedures, Locals, Arg_List, Parent, CurrentState, On_Stack1, Label_Counter, Label_Counter1, _),
	(
		(	
			%rekurencja
			Id = AcId,
			!,
			Off is Frame_Size + On_Stack +  AcFrame_Size,
			Result3 = [[const, swapa, load, swapd],[0xfffe],[const,add,swapa,load], [Off], 
					[swapa,const,add,swapa],[N],[store,swapa,swapd,const],[1],[add,swapa,const,store],
					[label(Label_Counter1)],[const,jump,nop,nop],[addr(Id,AcNlevel)],label(Label_Counter1)],
			append_all([Result1, Result2, Result3],Result),
			Label_Counter_Res is Label_Counter1 + 1
		) ;

		(
			% Procedura lokalna
			member((Id,Address,_,_,_,_), Procedures),
			!,
			Offset is Frame_Size + On_Stack,
			OffsetRet is N + 1,
			Result3 = [[const,swapa,load,swapd],[0xfffe],[const,add,swapa,nop],[Offset],[const,add,swapa,store],[N],
					   [const,add,swapa,const],[OffsetRet],[label(ReturnAddress)],[store,const,jump,nop],[Address],
					   label(ReturnAddress)],
			ReturnAddress = Label_Counter1,
			Label_Counter_Res is Label_Counter1 + 1,
			append_all([Result1, Result2, Result3],Result)
		) ;
		(
			%wywolywanie funkcji o wyzszym poziomie zagniezdzenia

			Off is Frame_Size + On_Stack, 
			Result3 = [[const,swapa,load,swapd],[0xfffe],[const,add,nop,nop],[Off]],
			find_proc_address(Parent ,Id, Result4, Address), % ACC = adres ktory chce zapisac jako adres ramki rodzica


			Offset = N,
			ReturnAddress = Label_Counter1,

			Result5 = [[swapd,const,swapa,load],[0xfffe],[swapa,const,swapd,swapa],[Offset],[add,swapa,store,swapa],
					   [swapd,const,add,swapa],[1],[const,store,const,jump],[label(ReturnAddress)],[Address],label(ReturnAddress)],
			Label_Counter_Res is Label_Counter1 + 1,
			append_all([Result1, Result2, Result3, Result4, Result5],Result)
		)
	).



% predykat oblicza wartosc argumentow procedury i wrzuca je na stos
compute_args([], [], _, _, _, _, _, _, Label_Counter, Label_Counter, 0).

compute_args([Arg | Tail], Result, Procedures, Locals, Arg_List, Parent, CurrentState, On_Stack, Label_Counter, Label_Counter_Res, Offset) :-
	compute_value(Arg, Result1, Procedures, Locals, Arg_List, Parent, CurrentState, On_Stack, Label_Counter, Label_Counter1),
	Result2 = [[swapd, const, swapa, load], [0xfffe], [swapd,swapa,const,add],[Offset1],[swapa,store,nop,nop] | Result21],
	compute_args(Tail, Result21, Procedures, Locals, Arg_List, Parent, CurrentState, On_Stack, Label_Counter1, Label_Counter_Res, Offset1),
	Offset is Offset1 + 1,
	append_all([Result1, Result2], Result).


find_variable_address(var(Id), Result, Locals, Arg_List, Parent, CurrentState, On_Stack) :-
	(	%zmienna lokalna
		member((Id, Offset1), Locals),!,
		Offset is Offset1 + On_Stack,
		Result = [[const, swapa, load, swapd],[0xfffe],[const,swapd,add,nop],[Offset]]
	) ;
	(	%argument
		member((Id, Offset1), Arg_List),!,
		Offset is Offset1 + On_Stack,
		Result = [[const, swapa, load, swapd],[0xfffe],[const,swapd,add,nop],[Offset]]
	) ;
	(	%zmiennej trzeba szukac w wyzszych procedurach.Przez wyzszych mam na mysli procedury o nizszym poziomie zagniezdzenia
		CurrentState = (_, AcFrame_Size, _),
		Offset is On_Stack + AcFrame_Size,
		Result = [[const, swapa, load, swapd],[0xfffe],[const,add,swapa,load],[Offset] | Tail],
		find_address(Id, Tail, Parent)
	).


find_address(Id, Code, [ (_, _, _, Local_Vars, _) | _] ) :-
	member((Id, Offset), Local_Vars),
	!,
	Code = [[swapd,const,add,nop],[Offset]].
find_address(Id, Code, [ (_, Frame_Size, _, _, _) | Parent] ) :-
	Code = [[swapd, const, add, swapa],[Frame_Size], [load,nop,nop,nop] | Tail],
	find_address(Id, Tail, Parent).




find_proc_address([(Id, Frame_Size, Nesting_Level, _, _) | _] ,Id, Code, addr(Id, Nesting_Level)) :-
	!, 
	Code = [[swapd, const, add, swapa],[Frame_Size],[load,nop,nop,nop]].

find_proc_address([(_,_,_,_, Procedures) | _], Id, [], Address) :-
	member((Id, Address, _, _, _, _), Procedures),
	!.

find_proc_address([(_, Frame_Size, _, _, _) | T], Id, Code, Address) :-
	Code = [[swapd,const,add,swapa],[Frame_Size],[load,nop,nop,nop] | X],
	find_proc_address(T, Id, X, Address).

push(Code) :-
	Code = [push_begin,[swapd,const,swapa,load], [0xfffe], [swapd,swapa,const,swapd],[1],[sub,swapa,store,const],[0xfffe],[swapa,store,nop,nop],push_end].

pop(Code) :- 
	Code = [pop_begin,[swapd,const,swapa,load],[0xfffe], [swapd,swapa,const,add],[1],[swapa,swapd,const,swapa],[0xfffe],
			[store,swapd,swapa,const],[1],[swapd,sub,swapa,swapd],[load,nop,nop,nop],pop_end].
% pop dziala przy zalozeniu ze w akumulatorze jest cos waznego.Po wykonaniu kodu,stara wartosc umieszczana jest w DATA a wartosc
% sciagana ze stosu w akumulatorze.