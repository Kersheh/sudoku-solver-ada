with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Sudoku is
  args: constant integer := argument_count;
  type sudoku is array(integer range 0 .. 8, integer range 0 .. 8) of character;
  current_board: sudoku;

  -- function to import board from file
  function load_board(file_in: string) return sudoku is
    input: file_type;
    i, j: integer := 0;
    read_in: character;
    board: sudoku;
  begin
    open(file => input, mode => in_file, name => file_in);
    while not end_of_file(input) loop
      if end_of_line(input) then
        skip_line(input);
        i := i + 1;
        j := 0;
      else
        get_immediate(input, read_in);
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
    for i in integer range 0 .. 8 loop
      put("|");
      for j in integer range 0 .. 8 loop
        put(board(i,j));
        if (j + 1) mod 3 = 0 then
          put("|");
        else
          put(" ");
        end if;
      end loop;
      new_line;
      if (i + 1) mod 3 = 0 then
        put_line("+-----+-----+-----+");
      end if;
    end loop;
  end print_sudoku;
  -- procedure to output board to file
  procedure output_sudoku(file_out: in string; board: in sudoku) is
    output: file_type;
  begin
    create(file => output, mode => out_file, name => file_out);
    put_line(output, "+-----+-----+-----+");
    for i in integer range 0 .. 8 loop
      put(output, "|");
      for j in integer range 0 .. 8 loop
        put(output, board(i,j));
        if (j + 1) mod 3 = 0 then
          put(output, "|");
        else
          put(output, " ");
        end if;
      end loop;
      new_line(output);
      if (i + 1) mod 3 = 0 then
        put_line(output, "+-----+-----+-----+");
      end if;
    end loop;
  end output_sudoku;

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
        get_line(filename_out, last);
        get_line(filename_out, last);
      end if;
    end loop;
    if output_option = 1 then
      print_sudoku(current_board);
    end if;
    if output_option = 2 then
      output_sudoku(filename_out(1 .. last), current_board);
    end if;
  end;
end Sudoku;