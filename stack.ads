package stack is
  type sudoku_board is array(integer range 1 .. 9, integer range 1 .. 9) of integer;
  procedure push(x: in sudoku_board);
  procedure pop(x: out sudoku_board);
  function stack_is_empty return Boolean;
  function stack_top return sudoku_board;
  procedure reset_stack;
end stack;