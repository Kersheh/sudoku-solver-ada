-- Modified code provided by Michael Wirth
with Ada.Text_IO; use Ada.Text_IO;
package body stack is
  empty: sudoku_board;

  type list is array(1 .. 100) of sudoku_board;
  type sudoku_stack is
    record
      item: list;
      top: natural := 0;
    end record;
  st: sudoku_stack;

  procedure push(x: in sudoku_board) is
  begin
    if st.top = 100 then
      put_line("stack is full");
    else
      st.top := st.top + 1;
      st.item(st.top) := x;
    end if;
  end push;

  procedure pop(x: out sudoku_board) is
  begin
    if st.top = 0 then
      put_line("stack is empty");
    else
      x := st.item(st.top);
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