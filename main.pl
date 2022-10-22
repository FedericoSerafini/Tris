play :-
  write('\n'),
  Board =
  [
    (1, 1, _), (1, 2, _), (1, 3, _),
    (2, 1, _), (2, 2, _), (2, 3, _),
    (3, 1, _), (3, 2, _), (3, 3, _)
  ],
  computer_turn(Board, _Board_out).

player :-
  write('\n************************\n\n'),
  Board =
  [
    (1, 1, _), (1, 2, _), (1, 3, _),
    (2, 1, _), (2, 2, _), (2, 3, _),
    (3, 1, _), (3, 2, _), (3, 3, _)
  ],
  print_board(Board),
  player_turn(Board, _Board_out).

computer :-
  write('\n************************\n\n'),
  Board =
  [
    (1, 1, _), (1, 2, _), (1, 3, _),
    (2, 1, _), (2, 2, _), (2, 3, _),
    (3, 1, _), (3, 2, _), (3, 3, _)
  ],
  computer_turn(Board, _Board_out).

computer_turn(Board_in, Board_out) :-
  (turn_simulation(Board_in, 'O', Board_list)
  ->
    board_list_val_max(Board_list, 'O', Board_out, _Val_out),
    writeln('Computer turn:'),
    print_board(Board_out),
    ((member((_I1, _I2, X), Board_in), var(X))
    ->
      (win_chk(Board_out, 'O')
      ->
        writeln('Computer wins!'),
        player
      ;
        player_turn(Board_out, _)
      )
    ;
      writeln('Game ended in a draw!'),
      computer
    )
  ;
    writeln('Game ended in a draw!'),
    computer
  ).

player_turn(Board_in, Board_out) :-
  ((member((_I1, _I2, V), Board_in), var(V))
  ->
    writeln('Your turn.'),
    read_row(Row),
    read_col(Col),
    Board_out = Board_in,
    (member((Row, Col, X), Board_out)
    ->
      (var(X)
      ->
        X = 'X',
        print_board(Board_out),
        (win_chk(Board_out, 'X')
        ->
          writeln('You win!'),
          computer
        ;
          (win_chk(Board_out, 'O')
          ->
            writeln('Computer wins!'),
            player
          ;
            computer_turn(Board_out, _)
          )
        )
      ;
        writeln('Input error: invalid index pair.'),
        player_turn(Board_in, _)
      )
    ;
      writeln('Input error: invalid index pair.'),
      player_turn(Board_in, _)
    )
  ;
    writeln('Game ended in a draw!'),
    player
  ).

turn_simulation(Board, S, Board_list) :-
  setof(B,
        I1^I2^Board^(
          B = Board,
          member((I1, I2, X), Board),
          var(X),
          X = S
        ),
        Board_list).

board_value(Board_in, S, Val) :-
  (win_chk(Board_in, 'X')
  ->
    Val = -1
  ;
    (win_chk(Board_in, 'O')
    ->
      Val = 1
    ;
      ((member((_I1, _I2, V), Board_in), var(V))
      ->
        (S == 'X'
        ->
          turn_simulation(Board_in, 'O', Board_list),
          board_list_val_max(Board_list, 'O', _Board_out, Val)
        ;
          turn_simulation(Board_in, 'X', Board_list),
          board_list_val_min(Board_list, 'X', _Board_out, Val)
        )
      ;
        Val = 0
      )
    )
  ).

board_list_val_min(Board_list, S, Board_out, Val_out) :-
  board_list_val_min_loop(Board_list, S, [], 100, Board_out, Val_out).
board_list_val_min_loop([], _S, Board_old, Val_old, Board_out, Val_out) :-
  !,
  Board_out = Board_old,
  Val_out = Val_old.
board_list_val_min_loop([Board|T], S, Board_old, Val_old, Board_out, Val_out) :-
  board_value(Board, S, Val),
  random(0, 2, R),
  (R == 0
  ->
    (Val < Val_old
    ->
      board_list_val_min_loop(T, S, Board, Val, Board_out, Val_out)
    ;
      board_list_val_min_loop(T, S, Board_old, Val_old, Board_out, Val_out)
    )
  ;
    (Val =< Val_old
    ->
      board_list_val_min_loop(T, S, Board, Val, Board_out, Val_out)
    ;
      board_list_val_min_loop(T, S, Board_old, Val_old, Board_out, Val_out)
    )
  ).

board_list_val_max(Board_list, S, Board_out, Val_out) :-
  board_list_val_max_loop(Board_list, S, [], -100, Board_out, Val_out).
board_list_val_max_loop([], _S, Board_old, Val_old, Board_out, Val_out) :-
  !,
  Board_out = Board_old,
  Val_out = Val_old.
board_list_val_max_loop([Board|T], S, Board_old, Val_old, Board_out, Val_out) :-
  board_value(Board, S, Val),
  random(0, 2, R),
  (R == 0
  ->
    (Val > Val_old
    ->
      board_list_val_max_loop(T, S, Board, Val, Board_out, Val_out)
    ;
      board_list_val_max_loop(T,S, Board_old, Val_old, Board_out, Val_out)
    )
  ;
    (Val >= Val_old
    ->
      board_list_val_max_loop(T, S, Board, Val, Board_out, Val_out)
    ;
      board_list_val_max_loop(T,S, Board_old, Val_old, Board_out, Val_out)
    )
  ).

win_chk(Board, S) :-
  (
    row_tris(Board, S)
  ;
    col_tris(Board, S)
  ;
    diag_tris(Board, S)
  ).


row_tris([(_I1, _I2, X1_1), (_I3, _I4, X1_2), (_I5, _I6, X1_3)|R], S) :-
  ((X1_1 == S, X1_2 == S, X1_3 == S)
  ->
    true
  ;
    row_tris(R, S)
  ).

col_tris(Board, S) :-
  Board =
  [
    (1, 1, X1_1), (1, 2, X1_2), (1, 3, X1_3),
    (2, 1, X2_1), (2, 2, X2_2), (2, 3, X2_3),
    (3, 1, X3_1), (3, 2, X3_2), (3, 3, X3_3)
  ],
  (
    (X1_1 == S, X2_1 == S, X3_1 == S)
  ;
    (X1_2 == S, X2_2 == S, X3_2 == S)
  ;
    (X1_3 == S, X2_3 == S, X3_3 == S)
  ).

diag_tris(Board, S) :-
  Board =
  [
    (1, 1, X1_1), (1, 2, _X1_2), (1, 3, X1_3),
    (2, 1, _X2_1), (2, 2, X2_2), (2, 3, _X2_3),
    (3, 1, X3_1), (3, 2, _X3_2), (3, 3, X3_3)
  ],
  (
    (X1_1 == S, X2_2 == S, X3_3 == S)
  ;
    (X1_3 == S, X2_2 == S, X3_1 == S)
  ).

print_board([]) :-
  !,
  write('\n\n').
print_board([C1, C2, C3|R]) :-
  print_row([C1, C2, C3]),
  print_board(R).

print_board_list([], _N) :-
  !.
print_board_list([H|R], N) :-
  writeln(N),
  print_board(H),
  N_new is N+1,
  print_board_list(R, N_new).

print_row([]) :-
  !,
  write('\n').
print_row([H|R]) :-
  H = (_I1, _I2, X),
  (var(X)
  ->
    write('.')
  ;
    write(X)
  ),
  write(' '),
  print_row(R).

read_row(Row) :-
  write('Insert row index: '),
  read_atom(Row),
  ((Row < 1 ; 3 < Row)
  ->
    writeln('Row index out of bounds.'),
    read_row(_Row)
  ;
    true
  ).

read_col(Col) :-
  write('Insert col index: '),
  read_atom(Col),
  ((Col < 1 ; 3 < Col)
  ->
    writeln('Col index out of bounds.'),
    read_col(_Col)
  ;
    true
  ).

%%%% legge sequenza di caratteri termaxata da '\n' da std input,
%%%% memorizza ASCII corrispondente in lista L
read_ascii_list(L) :-
    get0(C),
    read_ascii_list_loop(C, L).

read_ascii_list_loop(C, [C|R]) :-
    C \== 10, % codice ASCII per \n %
    read_ascii_list(R).

read_ascii_list_loop(10, []).

%%%% traduce lista L di codici ASCII nell'atomo A corrispondente
read_atom(A) :-
    read_ascii_list(L),
    name(A, L).
