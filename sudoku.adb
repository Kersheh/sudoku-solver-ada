with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

procedure Sudoku is
  args: constant integer := argument_count;
  type sudoku is array(integer range 1 .. 9, integer range 1 .. 9) of integer;
  current_board: sudoku;

  -- function to import board from file
  function load_board(file_in: string) return sudoku is
    input: file_type;
    i, j: integer := 1;
    read_in: integer;
    board: sudoku;
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
  procedure print_sudoku(board: in sudoku) is
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
  procedure output_sudoku(file_out: in string; board: in sudoku) is
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
  function is_blank_squares(board: sudoku) return boolean is
  begin
    for i in integer range 1 .. 9 loop
      for j in integer range 1 .. 9 loop
        if board(i, j) = 0 then
          return true;
        end if;
      end loop;
    end loop;
    return false;
  end is_blank_squares;

  -- boolean function checks if value is in current row
  function in_row(num: integer; row: integer; board: sudoku) return boolean is
  begin
    for i in integer range 1 .. 9 loop
      if board(row, i) = num then
        return true;
      end if;
    end loop;
    return false;
  end in_row;

  -- boolean function checks if value is in current column
  function in_column(num: integer; column: integer; board: sudoku) return boolean is
  begin
    for i in integer range 1 .. 9 loop
      if board(i, column) = num then
        return true;
      end if;
    end loop;
  return false;
  end in_column;

  -- boolean function checks if value is in 9x9 square
  function in_square(num: integer; row: integer; column: integer; board: sudoku) return boolean is
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
    elsif row < 7 then
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
  function is_legal(num: integer; x: integer; y: integer; board: sudoku) return boolean is
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

  -- recursive function attempts to solve sudoku board
  function solve_sudoku(board: in out sudoku) return boolean is
  begin
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
    if not solve_sudoku(current_board) then
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