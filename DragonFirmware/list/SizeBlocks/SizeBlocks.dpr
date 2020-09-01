program SizeBlocks;

{$APPTYPE CONSOLE}

uses
  SysUtils,Classes,Contnrs,StrUtils;

TYPE
  TModule  	= class(TObject)
    Name	: STRING;
    ModStart	: WORD;
    ModEnd	: WORD;
    ModLen	: WORD;
    Constructor Create(CName	: STRING;
                       CStart	: WORD;
                       CEnd	: WORD);
  END;


VAR	SymbolList	: TObjectList;


Constructor TModule.Create(CName	: STRING;
                           CStart	: WORD;
                           CEnd		: WORD);

BEGIN;
  INHERITED Create;

  Name:=CName;
  ModStart:=CStart;
  ModEnd:=CEnd;
  ModLen:=ModEnd-ModLen;
END;

FUNCTION GetAddress(Line	: STRING) : WORD;

CONST	Marker		= '(';
	EndMarker       = ')';

VAR	MarkerPos	: INTEGER;
   	EndMarkerPos	: INTEGER;
        HexStr		: STRING;

BEGIN;
  Result:=0;

  MarkerPos:=Pos(Marker,Line);
  EndMarkerPos:=PosEx(EndMarker,Line,MarkerPos);

  IF (EndMarkerPos > MarkerPos) THEN
  BEGIN;
    HexStr:=Copy(Line,MarkerPos+1,(EndMarkerPos-MarkerPos)-1);
    Result:=StrToInt(HexStr)
  END;
END;

FUNCTION GetSymbol(Line	: STRING) : STRING;

VAR	SpacePos	: INTEGER;

BEGIN;
  SpacePos:=Pos(' ',Line);

  IF (SpacePos<>0) THEN
    Result:=Copy(Line,1,SpacePos-1)
  ELSE
    Result:=Line;
END;

FUNCTION PadTo(Line	: STRING;
	       NewLen	: INTEGER) : STRING;

BEGIN;
  Result:=Line;

  WHILE (Length(Result) < NewLen) DO
    Result:=Result+' ';
END;

FUNCTION Compare(List: TStringList; Index1, Index2: Integer): Integer;

BEGIN;
  IF (Word(List.Objects[Index1]) = Word(List.Objects[Index2])) THEN
    Result:=0
  ELSE IF (Word(List.Objects[Index1]) > Word(List.Objects[Index2])) THEN
    Result:=1
  ELSE
    Result:=-1;
END;

FUNCTION CompareSize(Item1	: Pointer;
		     Item2	: Pointer) : INTEGER;

BEGIN;
END;

FUNCTION CompareStart(Item1	: Pointer;
		      Item2	: Pointer) : INTEGER;

BEGIN;
END;

PROCEDURE ShowResults(SortBySize	: BOOLEAN);

VAR	LineNo	: INTEGER;
    	Entry	: TModule;

BEGIN;
  IF (SortBySize) THEN
    SymbolList.Sort(CompareSize)
  ELSE
    SymbolList.Sort(CompareStart);

  FOR LineNo:=0 TO (SymbolList.Count-1) DO
  BEGIN;
    Entry:=TModule(SymbolList.Items[LineNo]);
    WriteLn(Format('%s start:%4.4X end:%4.4X length:%4.4X',[PadTo(Entry.Name,20),Entry.ModStart,Entry.ModEnd,Entry.ModLen]));
  END;
  WriteLn;
END;

PROCEDURE Process(InFileName	: STRING);

CONST	Marker		= '__';
	EndMarker       = '_end';

VAR	Lines		: TStringList;
	Symbols		: TStringList;
        LineNo		: INTEGER;
        Line		: STRING;
        EndIndex	: INTEGER;
        Symbol		: STRING;
        Address		: WORD;
        EndAddress	: WORD;
        Module		: TModule;

BEGIN;
  Lines:=TStringList.Create;
  Symbols:=TStringList.Create;
  TRY
    Lines.LoadFromFile(InFileName);

    // Extract the symbols
    FOR LineNo:=0 TO (Lines.Count-1) DO
    BEGIN;
      Line:=Trim(Lines.Strings[LineNo]);
      IF (Pos(Marker,Line)=1) THEN
      BEGIN;
        Symbol:=GetSymbol(Line);
        Address:=GetAddress(Line);
        //WriteLn(Format('%s start:%4.4X',[Symbol,Address]));
        Symbols.AddObject(Symbol,pointer(Address));
      END;
    END;

    Lines.Clear;
    Lines.AddStrings(Symbols);
    Symbols.Clear;

    // Extract start, end len
    FOR LineNo:=0 TO (Lines.Count-1) DO
    BEGIN;
      Symbol:=Lines.Strings[LineNo];
      Address:=WORD(Lines.Objects[LineNo]);

      IF (Pos(EndMarker,Symbol)=0) THEN
      BEGIN;
        EndIndex:=Lines.IndexOf(Symbol+EndMarker);
        IF (EndIndex > -1) THEN
        BEGIN;
          EndAddress:=WORD(Lines.Objects[EndIndex]);
          SymbolList.Add(TModule.Create(Symbol,Address,EndAddress));
        END
        ELSE
          WriteLn(Format('%s start:%4.4X, no %s%s found!',[PadTo(Symbol,20),Address,Symbol,EndMarker]));
      END;
    END;

    WriteLn('Sorted by size...');
    ShowResults(TRUE);

    WriteLn('Sorted by start address...');
    ShowResults(FALSE);

  FINALLY
    Lines.Free;
    Symbols.Free;
  END;
END;

begin
  { TODO -oUser -cConsole Main : Insert code here }
  SymbolList:=TObjectList.Create;
  TRY
    SymbolList.OwnsObjects:=TRUE;

    IF (ParamCount > 0) THEN
      Process(ParamStr(1))
    ELSE
      WriteLn('Error must supply list file!');
  FINALLY
    SymbolList.Free;
  END;
end.
