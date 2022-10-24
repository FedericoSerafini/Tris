:- initialization(main, main).

main :-
  board_init(Board),
  computer_turn(Board, _Board_out).

board_init(Board) :-
  Board =
  [
    (1, 1, _), (1, 2, _), (1, 3, _),
    (2, 1, _), (2, 2, _), (2, 3, _),
    (3, 1, _), (3, 2, _), (3, 3, _)
  ].

computer_turn(Board_in, Board_out) :-
  (turn_simulation(Board_in, 'O', Board_list)
  ->
    board_list_val_max(Board_list, 'O', Board_out, _Val_out),
    writeln('Computer turn:'),
    board_print(Board_out),
    ((member((_I1, _I2, X), Board_in), var(X))
    ->
      (win_chk(Board_out, 'O')
      ->
        writeln('Computer wins!'),
        player_play_again
      ;
        player_turn(Board_out, _)
      )
    ;
      writeln('Game ended in a draw!'),
      computer_play_again
    )
  ;
    writeln('Game ended in a draw!'),
    computer_play_again
  ).

player_turn(Board_in, Board_out) :-
  ((member((_I1, _I2, V), Board_in), var(V))
  ->
    writeln('Your turn.'),
    row_read(Row),
    col_read(Col),
    Board_out = Board_in,
    (member((Row, Col, X), Board_out)
    ->
      (var(X)
      ->
        X = 'X',
        board_print(Board_out),
        (win_chk(Board_out, 'X')
        ->
          writeln('You win!'),
          computer_play_again
        ;
          (win_chk(Board_out, 'O')
          ->
            writeln('Computer wins!'),
            player_play_again
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
    player_play_again
  ).

% Get all the possible boards starting from Board_in and placing the symbol S.
turn_simulation(Board_in, S, Board_list) :-
  setof(B,
        I1^I2^Board_in^(
          B = Board_in,
          member((I1, I2, X), Board_in),
          var(X),
          X = S
        ),
        Board_list).

player_play_again :-
  write('\n************************\n\n'),
  board_init(Board),
  board_print(Board),
  player_turn(Board, _Board_out).

computer_play_again :-
  write('\n************************\n\n'),
  board_init(Board),
  computer_turn(Board, _Board_out).

/*
  Board_in evaluation:
    1 <==> 'O' wins:
   -1 <==> 'X' wins;
    0 otherwise.
*/
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

% Find the Board_out in Board_list with the minimum evaluation.
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

% Find the Board_out in Board_list with the maximum evaluation.
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

board_print([]) :-
  !,
  write('\n\n').
board_print([C1, C2, C3|R]) :-
  row_print([C1, C2, C3]),
  board_print(R).

board_print_list([], _N) :-
  !.
board_print_list([H|R], N) :-
  writeln(N),
  board_print(H),
  N_new is N+1,
  board_print_list(R, N_new).

row_print([]) :-
  !,
  write('\n').
row_print([H|R]) :-
  H = (_I1, _I2, X),
  (var(X)
  ->
    write('.')
  ;
    write(X)
  ),
  write(' '),
  row_print(R).

row_read(Row) :-
  write('Insert row index and press enter (exit with \'q\'): '),
  read_atom(Row),
  (Row == 'q'
  ->
    writeln('Bye!'),
    halt
  ;
    ((Row < 1 ; 3 < Row)
    ->
      writeln('Row index out of bounds.'),
      row_read(_Row)
    ;
      true
    )
  ).

col_read(Col) :-
  write('Insert col index and press enter (exit with \'q\'): '),
  read_atom(Col),
  (Col == 'q'
  ->
    writeln('Bye!'),
    halt
  ;
    ((Col < 1 ; 3 < Col)
    ->
      writeln('Col index out of bounds.'),
      col_read(_Col)
    ;
      true
    )
  ).

read_atom(A) :-
    read_ascii_list(L),
    name(A, L).

read_ascii_list(L) :-
    get0(C),
    read_ascii_list_loop(C, L).

read_ascii_list_loop(10, []) :-
  !.
read_ascii_list_loop(C, [C|R]) :-
    C \== 10,
    read_ascii_list(R).
