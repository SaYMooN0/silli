program BFInterpreter;

var
  leftTape : string;
  rightTape : string;
  currentCell : integer;

  inputCode : integer;
  
  programText : string;
  pc : integer;


procedure resetState;
begin
  pc := 0;
  leftTape := "";
  rightTape := "";
  currentCell := 0;
end;

procedure moveRight;
begin
  leftTape := stringConcat(leftTape, stringFromAsciiCode(currentCell));

  if stringLength(rightTape) = 0 then
    currentCell := 0
  else
  begin
    currentCell := asciiCodeFromString(stringCharAt(rightTape, 0));
    rightTape := stringRightFromIndex(rightTape, 1);
  end;
end;

procedure moveLeft;
var
  lastIndex : integer;
begin
  rightTape := stringConcat(stringFromAsciiCode(currentCell), rightTape);

  if stringLength(leftTape) = 0 then
    currentCell := 0
  else
  begin
    lastIndex := stringLength(leftTape) - 1;
    currentCell := asciiCodeFromString(stringCharAt(leftTape, lastIndex));
    leftTape := stringLeftUntilIndex(leftTape, lastIndex);
  end;
end;

procedure incrementCell;
begin
  currentCell := currentCell + 1;

  if currentCell > 255 then
    currentCell := 0;
end;

procedure decrementCell;
begin
  currentCell := currentCell - 1;

  if currentCell < 0 then
    currentCell := 255;
end;

procedure printCurrentCell;
begin
  printString(stringFromAsciiCode(currentCell));
end;

procedure readCurrentCell;
begin
  inputCode := readSingleCharAsAsciiCode();

  if inputCode < 0 then
    currentCell := 0
  else
    currentCell := inputCode;
end;

function findMatchingRightBracket(pos : integer; depth : integer) : integer;
var
  ch : string;
begin
  if pos >= stringLength(programText) then
    result := -1
  else
  begin
    ch := stringCharAt(programText, pos);

    if ch = "[" then
      result := findMatchingRightBracket(pos + 1, depth + 1)
    else
      if ch = "]" then
        if depth = 1 then
          result := pos
        else
          result := findMatchingRightBracket(pos + 1, depth - 1)
      else
        result := findMatchingRightBracket(pos + 1, depth);
  end;
end;

function findMatchingLeftBracket(pos : integer; depth : integer) : integer;
var
  ch : string;
begin
  if pos < 0 then
    result := -1
  else
  begin
    ch := stringCharAt(programText, pos);

    if ch = "]" then
      result := findMatchingLeftBracket(pos - 1, depth + 1)
    else
      if ch = "[" then
        if depth = 1 then
          result := pos
        else
          result := findMatchingLeftBracket(pos - 1, depth - 1)
      else
        result := findMatchingLeftBracket(pos - 1, depth);
  end;
end;

procedure jumpAfterMatchingRightBracket;
var
  bracketPos : integer;
begin
  bracketPos := findMatchingRightBracket(pc + 1, 1);

  if bracketPos < 0 then
  begin
    printLine("Runtime error: unmatched '['");
    pc := stringLength(programText);
  end
  else
    pc := bracketPos + 1;
end;

procedure jumpAfterMatchingLeftBracket;
var
  bracketPos : integer;
begin
  bracketPos := findMatchingLeftBracket(pc - 1, 1);

  if bracketPos < 0 then
  begin
    printLine("Runtime error: unmatched ']'");
    pc := stringLength(programText);
  end
  else
    pc := bracketPos + 1;
end;

procedure executeProgram;
var
  cmd : string;
begin
  if pc < stringLength(programText) then
  begin
    cmd := stringCharAt(programText, pc);

    if cmd = ">" then
    begin
      moveRight();
      pc := pc + 1;
      executeProgram();
    end
    else
      if cmd = "<" then
      begin
        moveLeft();
        pc := pc + 1;
        executeProgram();
      end
      else
        if cmd = "+" then
        begin
          incrementCell();
          pc := pc + 1;
          executeProgram();
        end
        else
          if cmd = "-" then
          begin
            decrementCell();
            pc := pc + 1;
            executeProgram();
          end
          else
            if cmd = "." then
            begin
              printCurrentCell();
              pc := pc + 1;
              executeProgram();
            end
            else
              if cmd = "," then
              begin
                readCurrentCell();
                pc := pc + 1;
                executeProgram();
              end
              else
                if cmd = "[" then
                begin
                  if currentCell = 0 then
                    jumpAfterMatchingRightBracket()
                  else
                    pc := pc + 1;

                  executeProgram();
                end
                else
                  if cmd = "]" then
                  begin
                    if currentCell <> 0 then
                      jumpAfterMatchingLeftBracket()
                    else
                      pc := pc + 1;

                    executeProgram();
                  end
                  else
                  begin
                    pc := pc + 1;
                    executeProgram();
                  end;
  end;
end;

procedure repl;
begin
  printString("Input your bf code or \"exit\" to exit programm: \n");
  programText := readLine();

  if programText = "exit" then
    printLine("Bye")
  else
  begin
    resetState();
    executeProgram();
    printLine("");
    repl();
  end;
end;

begin
  repl();
end.