-- Modified code provided by Michael Wirth
with Ada.Text_IO; use Ada.Text_IO;
package body stack is
  empty: sudoku_board;
  
  type list_boards is array(1 .. 4096) of sudoku_board;
  type sudoku_stack is
    record
      item: list_boards;
      top: natural := 0;
    end record;
  st: sudoku_stack;

  procedure push(board: in sudoku_board) is
  begin
    if st.top = 4096 then
      put_line("stack is full");
    else
      st.top := st.top + 1;
      st.item(st.top) := board;
    end if;
  end push;

  procedure pop(board: out sudoku_board) is
  begin
    if st.top = 0 then
      put_line("stack is empty");
    else
      board := st.item(st.top);
      st.top := st.top - 1;
    end if;
  end pop;

  function stack_is_empty return Boolean is
  begin
    return st.top = 0;
  end stack_is_empty;

  function stack_top return sudoku_board is
  begin
    if st.top = 0 then
      put_line("stack is empty");
      for i in integer range 1 .. 9 loop
        for j in integer range 1 .. 9 loop
          empty(i, j) := 0;
        end loop;
      end loop;
      return empty;
    else
      return st.item(st.top);
    end if;
  end stack_top;

  procedure reset_stack is
  begin
    st.top := 0;
  end reset_stack;
end stack;