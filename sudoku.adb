with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with stack; use stack;

procedure Sudoku is
  args: constant integer := argument_count;

  -- function to import board from file
  function load_board(file_in: string) return sudoku_board is
    input: file_type;
    i, j: integer := 1;
    read_in: integer;
    board: sudoku_board;
  begin
    open(file => input, mode => in_file, name => file_in);
    while not end_of_file(input) loop
      if end_of_line(input) then
        skip_line(input);
        i := i + 1;
        j := 1;
      else
        get(input, read_in, 1);
        board(i, j) := read_in;
        j := j + 1;
      end if;
    end loop;
    close(input);
    return board;
  end load_board;

  -- procedure to print board
  procedure print_sudoku(board: in sudoku_board) is
  begin
    put_line("+-----+-----+-----+");
    for i in integer range 1 .. 9 loop
      put("|");
      for j in integer range 1 .. 9 loop
        put(trim(integer'image(board(i, j)), Ada.Strings.Left));
        if j mod 3 = 0 then
          put("|");
        else
          put(" ");
        end if;
      end loop;
      new_line;
      if i mod 3 = 0 then
        put_line("+-----+-----+-----+");
      end if;
    end loop;
  end print_sudoku;

  -- procedure to output board to file
  procedure output_sudoku(file_out: in string; board: in sudoku_board) is
    output: file_type;
  begin
    create(file => output, mode => out_file, name => file_out & ".txt");
    put_line(output, "+-----+-----+-----+");
    for i in integer range 1 .. 9 loop
      put(output, "|");
      for j in integer range 1 .. 9 loop
        put(output, trim(integer'image(board(i, j)), Ada.Strings.Left));
        if j mod 3 = 0 then
          put(output, "|");
        else
          put(output, " ");
        end if;
      end loop;
      new_line(output);
      if i mod 3 = 0 then
        put_line(output, "+-----+-----+-----+");
      end if;
    end loop;
  end output_sudoku;

  -- boolean function checks for blank squares throughout puzzle
  function is_blank_squares(board: sudoku_board; x: in out integer; y: in out integer) return boolean is
  begin
    for i in integer range 1 .. 9 loop
      for j in integer range 1 .. 9 loop
        if board(i, j) = 0 then
          x := i;
          y := j;
          return true;
        end if;
      end loop;
    end loop;
    return false;
  end is_blank_squares;

  -- boolean function checks if value is in current row
  function in_row(num: integer; row: integer; board: sudoku_board) return boolean is
  begin
    for i in integer range 1 .. 9 loop
      if board(row, i) = num then
        return true;
      end if;
    end loop;
    return false;
  end in_row;

  -- boolean function checks if value is in current column
  function in_column(num: integer; column: integer; board: sudoku_board) return boolean is
  begin
    for i in integer range 1 .. 9 loop
      if board(i, column) = num then
        return true;
      end if;
    end loop;
  return false;
  end in_column;

  -- boolean function checks if value is in 9x9 square
  function in_square(num: integer; row: integer; column: integer; board: sudoku_board) return boolean is
    x_min, x_max, y_min, y_max: integer;
  begin
    -- retrieve subsquare range
    if row < 4 then
      x_min := 1; x_max := 3;
    elsif row < 7 then
      x_min := 4; x_max := 6;
    else
      x_min := 7; x_max := 9;
    end if;
    if column < 4 then
      y_min := 1; y_max := 3;
    elsif column < 7 then
      y_min := 4; y_max := 6;
    else
      y_min := 7; y_max := 9;
    end if;
    for i in integer range x_min .. x_max loop
      for j in integer range y_min .. y_max loop
        if board(i, j) = num then
          return true;
        end if;
      end loop;
    end loop;
  return false;
  end in_square;

  -- boolean function checks if value is legal in cell
  function is_legal(num: integer; x: integer; y: integer; board: sudoku_board) return boolean is
  begin
    -- return false if value is not 0
    if board(x, y) /= 0 then
      return false;
    end if;
    if not in_row(num, x, board) and not in_column(num, y, board) and not in_square(num, x, y, board) then
      return true;
    end if;
    return false;
  end is_legal;

  -- function solves cell if only one number if possible
  function solve_single_cell(x: integer; y: integer; board: sudoku_board) return integer is
    answer_found: boolean := false;
    answer: integer;
  begin
    -- return cell value if already filled
    if board(x, y) /= 0 then
      return board(x, y);
    end if;
    for i in integer range 1 .. 9 loop
      if is_legal(i, x, y, board) then
        -- if answer found for cell, more than one answer if valid; return 0
        if answer_found then
          return 0;
        end if;
        answer_found := true;
        answer := i;
      end if;
    end loop;
    -- return answer; return 0 if no answer found
    if answer_found then
      return answer;
    end if;
    return 0;
  end solve_single_cell;

  -- function solves immediate solvable cells; returns false if no changes made
  function solve_cells(board: in out sudoku_board) return boolean is
    changes_made: boolean := false;
  begin
    for i in integer range 1 .. 9 loop
      for j in integer range 1 .. 9 loop
        if board(i, j) = 0 then
          board(i, j) := solve_single_cell(i, j, board);
          if board(i, j) /= 0 then
            changes_made := true;
          end if;
        end if;
      end loop;
    end loop;
    return changes_made;
  end solve_cells;

  -- function creates and returns an identical copy of a sudoku board
  function copy_board(board: sudoku_board) return sudoku_board is
    new_board: sudoku_board;
  begin
    for i in integer range 1 .. 9 loop
      for j in integer range 1 .. 9 loop
        new_board(i, j) := board(i, j);
      end loop;
    end loop;
    return new_board;
  end copy_board;

  -- recursive function attempts to solve sudoku board
  function solve_sudoku(x: in integer; y: in integer; board: in out sudoku_board) return boolean is
    current_x: integer := x;
    current_y: integer := y;
  begin
    -- loop through solving immediate solvable cells
    while solve_cells(board) loop
      null;
    end loop;
    -- check if puzzle is complete
    if not is_blank_squares(board, current_x, current_y) then
       return true;
    end if;
    stack.push(board); -- push current board state onto stack
    for i in integer range 1 .. 9 loop
      if is_legal(i, current_x, current_y, board) then
        board(current_x, current_y) := i;
        if solve_sudoku(current_x, current_y, board) then
          return true;
        end if;
        stack.pop(board);
      end if;
    end loop;
    return false;
  end solve_sudoku;

-- main process
begin
  -- check args
  if args /= 1 then
    put_line("usage: ./sudoku [file_input]");
    return;
  end if;
  -- set file input
  declare
    filename_in: string := argument(1);
    output_option: integer := -1;
    filename_out: string(1 .. 64);
    last: natural; -- tracks filename_out length
    current_board: sudoku_board;
  begin
    -- load input file
    current_board := load_board(filename_in);

    put_line("Sudoku Puzzle Solver");
    put_line("Starting Puzzle:");
    print_sudoku(current_board);
    -- prompt user for output
    while output_option /= 1 and output_option /= 2 loop
      put("Output solved puzzle to stdin (1) or file (2)? ");
      get(output_option);
      if output_option /= 1 and output_option /= 2 then
        put_line("Invalid input.");
      end if;
      if output_option = 2 then
        put_line("Output file name (auto-appends .txt format): ");
        -- cannot figure out why a single get_line retrieves a blank string
        -- while repeating get_line works at retrieving the string once
        get_line(filename_out, last);
        get_line(filename_out, last);
      end if;
    end loop;
    -- solve sudoku
    if not solve_sudoku(1, 1, current_board) then
      put_line("Puzzle is impossible to solve.");
    else
      -- print completed puzzle to stdin
      if output_option = 1 then
        put_line("Completed Puzzle:");
        print_sudoku(current_board);
      end if;
      -- print completed puzzle to .txt file
      if output_option = 2 then
        output_sudoku(filename_out(1 .. last), current_board);
      end if;
    end if;
  end;
end Sudoku;