unit PASCAL_FC_A;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.StdCtrls, CisloRadku;

type
  TForm1 = class(TForm)
    OpenDialog1: TOpenDialog;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    New1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    SaveAs1: TMenuItem;
    Print1: TMenuItem;
    PrintSetup1: TMenuItem;
    Exit1: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    Edit1: TMenuItem;
    Undo1: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    PasteSpecial1: TMenuItem;
    Find1: TMenuItem;
    Replace1: TMenuItem;
    GoTo1: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    Help1: TMenuItem;
    Contents1: TMenuItem;
    Index1: TMenuItem;
    Commands1: TMenuItem;
    Procedures1: TMenuItem;
    Keyboard1: TMenuItem;
    SearchforHelpOn1: TMenuItem;
    Tutorial1: TMenuItem;
    HowtoUseHelp1: TMenuItem;
    About1: TMenuItem;
    Memo1: TMemo;
    ranslate1: TMenuItem;
    ranslate2: TMenuItem;
    Run1: TMenuItem;
    Run2: TMenuItem;
    Memo2: TMemo;
    SaveDialog1: TSaveDialog;
    FindDialog1: TFindDialog;
    ReplaceDialog1: TReplaceDialog;
    PrinterSetupDialog1: TPrinterSetupDialog;
    PrintDialog1: TPrintDialog;
    procedure Open1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure ranslate2Click(Sender: TObject);
    procedure Run2Click(Sender: TObject);
    procedure New1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure SaveAs1Click(Sender: TObject);
    function SaveFile (const SaveAs: Boolean): Boolean;
    procedure Cut1Click(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure Paste1Click(Sender: TObject);
    procedure Find1Click(Sender: TObject);
    procedure Replace1Click(Sender: TObject);
    procedure Print1Click(Sender: TObject);
    procedure PrintSetup1Click(Sender: TObject);
    procedure FindDialogFind(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ReplaceDialog1Replace(Sender: TObject);
    procedure Undo1Click(Sender: TObject);
    procedure GoTo1Click(Sender: TObject);

  private
    { Private declarations }
      FSelPos: integer;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}
var
 NazevSouboru: string;
 Directory: string;
 FileStream: TFileStream;
 pmdfile: text;
 Source:string;
 LengthOfSource:integer;
 PositionOfSource:integer;
 Done:boolean;

 const

   (* @(#)globcons.i	4.1 10/24/89 *)

   alng=10; 		(* length of identifiers *)
   xmax = maxint;
   omax=200;            (* largest op-code for p-machine *)
   funmax = omax;	(* highest function number *)


(* impcons.i *)
(* BM 1 version *)


	target = 'IBM PC compatibles';

   maxmons=10;          (* maximum monitor in a program *)
   maxcapsprocs=10;     (* maximum exported procedures from a monitor *)
   intermax=10;         (* max no. of mapped ipc primitives *)
   tmax=150;            (* max size of symbol table *)
   bmax=50;             (* max size of block table *)
   amax=20;             (* max size of array table *)
   casemax=20;          (* max number of case labels or selects *)
   chanmax=20;          (* maximum size of channel table - gld *)
   cmax=2000;           (* max size of p-code array *)
   lmax=7;              (* max depth of block nesting *)
   smax=1500;           (* max size of string table *)
   rmax = 50;		(* real constant table limit *)
   etmax = 20;		(* enumeration type upper bounds table *)

   llng=121;		(* max source input line length *)
   tabstop=3;           (* for 1 implementation - gld *)
   tabchar = 9;

   fals = 0;
   tru = 1;
   charl=0;      	(* first legal ascii character *)
   charh=127;   	(* last legal ascii character *)

   intmax = 32767;	(* maximum integer on target *)
   intmsb = 16;		(* most sig. bit in target integer *)

   realmax = 1e38;	(* maximum real number on target
										   or host, whichever is smaller *)
   minreal = 1e-37;	(* smallest real (for division) *)
   emax = 38;		(* maximum real exponent on target *)
   emin = -emax;

   bsmsb = 7;		(* most sig. bit in target bitset *)

   impfiles = false;
   impmapping = false;
   imptiming = false;
   impreals = true;

   monvarsize = 2;
   protvarsize = 3;
   chansize=3;
   entrysize = 3;       (* space for a process entry point *)
   sfsize=6;            (* size of "frame" in a select statement *)


   bitsetsize = 1;
   intsize = 1;
   boolsize = 1;
   charsize = 1;
   semasize = 1;
   condvarsize = 1;
   synchrosize = 0;
   procsize = 1;
   enumsize = 1;
   realsize = 1;

   objalign = 1;
   pushdown = false;

(* interpreter-specific constants *)

   stepmax=8;
   statmax=200000;      (* maximum statements before "livelock *)

(* NOTE - make (stmax - (stkincr * pmax)) >= stkincr *)

   stmax=5000;
   stkincr=200;
   pmax=20;
   msb = 7;


   actrecsize = 5;	(* size of subprogram "housekeeping" block *)

type

	(* @(#)globtypes.i	4.7 11/8/91 *)

   opcode=(ldadr,ldval,ldind,updis,cobeg,coend,wait,signal,stfun,ixrec,
      jmp,jmpiz,for1up,for2up,mrkstk,callsub,ixary,ldblk,cpblk,
      ldcon,ifloat,readip,wrstr,wrval,stop,retproc,retfun,repadr,notop,
      negate,store,relequ,relneq,rellt,relle,relgt,relge,orop,
      add,sub,andop,mul,divop,modop,rdlin,wrlin,selec0,chanwr,
      chanrd,delay,resum,enmon,exmon,mexec,mretn,
      lobnd,hibnd,pref,sleap,
      procv,ecall,acpt1,acpt2,rep1c,rep2c,btest,enmap,wrfrm,w2frm,
      wrsfm,wrbas,power2,slabl,blokk,param,case1,case2,selec1,
      sinit,prtex,prtjmp,prtsel,prtslp,prtcnd);

   index = -xmax .. xmax;
   alfa = packed array[1..alng] of char;
   objekt = (konstant,variable,type1,prozedure,funktion,monproc,address,
             grdproc,xgrdproc);

   types = (notyp,ints,reals,bools,chars,arrays,records,
      semafors,channels,monvars,condvars,synchros,adrs,
      procs,entrys,enums,bitsets,
      protvars,protq);

   typset = set of types;

   fnametype = packed array[1..30] of char;

   order =
   packed record
      f: opcode;
      x: -lmax..+lmax;
      y: integer;
      instyp: types;
      line: integer
   end;
   orderarray = array[0..cmax] of order;

   objorder =
	packed record
		f: 0..omax;
		x: -lmax..lmax;
		y: integer;
		l: integer
	end;
   objorderarray = array[0..cmax] of objorder;

   tabrec =
	packed record
   		name: alfa;
   		link: index;
   		obj: objekt;
   		typ: types;
   		ref: index;
   		normal: boolean;
   		lev: 0..lmax;
   		taddr: integer;
		auxref: index
   	end;
   tabarray = array[0..tmax] of tabrec;

   atabrec =
   packed record
      	inxtyp,eltyp:types;
      	inxref,elref,low,high,elsize,size:index;
   end;
   atabarray = array[1..amax] of atabrec;

   btabrec =
   packed record
      	last,lastpar,psize,vsize:index;
      	tabptr: 0..tmax
   end;
   btabarray = array[1..bmax] of btabrec;

   stabarray = packed array[0..smax] of char;
   realarray = array[1..rmax] of real;

   intabrec =
   packed record
      tp: types;
      lv:  0..lmax;
      rf: integer;
      vector: integer;
      off:  integer;
      tabref: integer
   end;
   intabarray = array[1..intermax] of intabrec;



(* unixtypes.i *)

(* Pascal-FC "universal" compiler system *)
(* implementation-dependent type declaration for Unix *)


objcoderec =
	packed record
		fname:		fnametype;
		prgname:    alfa;
		gencode:    objorderarray;
		ngencode:   0..cmax;

		gentab:  tabarray;
		ngentab: 0..tmax;

		genatab:    atabarray;
		ngenatab: 0..amax;

		genbtab: btabarray;
		ngenbtab: 0..bmax;

		genstab: stabarray;
		ngenstab: 0..smax;
		genrconst: realarray;

    useridstart: 0..tmax;
    Length: Integer;
    Position: Integer;
  end;


  var

	(* @(#)globvars.i	4.4 6/16/92 *)

   filename: fnametype;
   progfile, listfile, objfile1: text;
   progname:alfa;
   lc,t,a,b,sx:integer;
   stantyps:typset;
   display:array[0..lmax] of integer;
   tab: tabarray;
   atab: atabarray;
   btab: btabarray;
   stab: stabarray;
	rconst: realarray;
	rnum: real;
	r, realindex: integer;
	e: integer;
   code: orderarray;
   ttt: 0..cmax;
	useridstart: 0..tmax;

   intab: intabarray;

   int: integer;
   simpletyps,bittyps, ipctyps: typset;

	success: boolean;

(* unixvars.i *)

(* implementation-dependent variable declarations for 1 *)

var
   objfile: objcoderec;
   obj:array of objcoderec;





function TForm1.SaveFile (const SaveAs: Boolean): Boolean;
begin
  Result := False;
  if SaveAs or (NazevSouboru = '') then begin
    SaveDialog1.FileName := NazevSouboru;
    if not SaveDialog1.Execute then Exit;
    Memo1.Lines.SaveToFile (SaveDialog1.Filename);
    NazevSouboru := SaveDialog1.Filename;

  end
  else
    Memo1.Lines.SaveToFile (NazevSouboru);
  Memo1.Modified := False;
  Memo1.ClearUndo;
  Result := True;

end;

 procedure TForm1.Undo1Click(Sender: TObject);
begin
  if Memo1.Focused then
    Memo1.Undo;

end;

(* @(#)pfcfront.i	5.2 12/1/92 *)

procedure pfcfront(var success: boolean);

(* "Universal" Pascal-FC compiler front end *)

label 99;

const

   nkw= 51;             (* number of reserved words recognised *)

type

   symbol =
     (intcon,realcon,charcon,string1,
      notsy,plus,minus,times,idiv,rdiv,imod,andsy,orsy,
      eql,neq,gtr,geq,lss,leq,
      lparent,rparent,lbrack,rbrack,comma,semicolon,
      period, shriek, query,
      colon,becomes,arrow,constsy,typesy,varsy,functionsy,
      proceduresy,processsy,arraysy,recordsy,channelsy,programsy,ident,
      beginsy,ifsy,casesy,repeatsy,whilesy,forsy, foreversy,
      endsy,elsesy,untilsy,ofsy,dosy,tosy,thensy,
      selectsy,whensy,prisy,termsy,nullsy,exportsy,monitorsy,atsy,
      offsetsy,insy,adrsy,timeoutsy,resourcesy,guardedsy,requeuesy,
      forwardsy,entrysy,acceptsy,providessy,replicatesy,percent,rbrace);


   symset = set of symbol;

   er =
		(erdec,erdup,erident,ertyp,erlparent,errparent,erlbrack,
		errbrack,ercolon,ersemi,erperiod,ereql,erbecomes,erprogram,
		erof,erthen,eruntil,erdo,erto,erbegin,erend,erselect,
		erexport,erreplicate,erpar,ervarpar,erparmatch, erchar,ersym,
		erstring,erlev,ernum,erassign,ercapsprocdecs,erinx,erent,
		ernotinproc,ermap,ertimetermelse,ercob,erfordec,
		erprovdec,ervar,erentmiss,ercasedup,erprocinrec,ersetlit,
		ernotprocvar,ersub,erconst,erentext,erentmatch,ernestacpt,
		eracptinproc,ernotingrdproc,eronlyingrdproc,ermustbeguarded,
		eronlyinres,ergrdcall);




   item =
      record
       typ:types;ref:index;
      end;

   keytabrec=
      record
         key: alfa;
         ksy: symbol
      end;

var


	linenum: integer;
	lineold, linenew: integer;
   sy:symbol;
   id:alfa;
   inum:integer;
   sleng:integer;
   ch:char;
   line:array[1..llng] of char;
   cc:integer;
   ll:integer;
   errs: set of er;
   errpos:integer;
   skipflag:boolean;
   constbegsys,typebegsys,blockbegsys,facbegsys,statbegsys:symset;
   keywords:array[1..nkw] of keytabrec;
   sps:array[char] of symbol;

   chantab: array[1..chanmax] of
   packed
   record
      eltyp: types;
      elref, elsize: index
   end;  (* chantab *)
   chan:  0..chanmax;              (* index to chantab  *)

   capsproctab: array [1..maxcapsprocs] of
      record
         name: alfa;
         foundec: boolean
      end;

   montab:
      record
	n: 0..maxmons;
        startadds: array[1..maxmons] of integer
      end;

   ncapsprocs: 0..maxcapsprocs;
   curcaps: 0..tmax;
   inguardedproc: boolean;
   numerror, negative: boolean;
   digit, base: integer;

   legalchars: set of char;
   incobegin, wascobegin: boolean;
	inprocessdec, inaloop: boolean;
	et: integer;
	labelnum: integer;
	internalnum:  integer;

	bounds: array[1..etmax] of
		record
			upper,
			lower:	integer
		end;



  procedure headermsg(var tofile: text);

   begin
         writeln(tofile, '- Pascal-FC for ',target,' - ');
         writeln(tofile, '- GNU Compiler Version P5.2');
         writeln(tofile);
         writeln(tofile, 'G L Davies  &  A Burns, University of York');
         writeln(tofile)
   end;  (* headermsg *)

   procedure initkeytab;

   (* set up table of keywords and sort *)

   var i: integer;

      procedure sort;

      (* sort table of keywords *)

      var
            swap: boolean;
            pass,j: integer;
            temp: keytabrec;


      begin  (* sort *)

         swap:=true;
         pass:=1;

         while swap and (pass < nkw) do
            begin
            swap:=false;

            for j:=1 to nkw - pass do
               if keywords[j].key > keywords[j + 1].key then
                  begin
                  swap := true;
                  temp := keywords[j];
                  keywords[j] := keywords[j + 1];
                  keywords[j + 1] := temp
                  end;

            pass:=pass + 1
            end  (*while loop*)

      end;  (*procedure sort*)


      procedure install(name: alfa; sym: symbol);

      begin
         with keywords[i] do
            begin
            key := name;
            ksy := sym
            end;
         i := i + 1
      end;  (* install *)


   begin  (* initkeytab *)
         i := 1;
         install('and       ',andsy);
         install('array     ',arraysy);
         install('begin     ',beginsy);
         install('channel   ',channelsy);
         install('cobegin   ',beginsy);
         install('coend     ',endsy);
         install('const     ',constsy);
         install('div       ',idiv);
         install('do        ',dosy);
         install('else      ',elsesy);
         install('end       ',endsy);
         install('export    ',exportsy);
         install('for       ',forsy);
         install('forever   ',foreversy);
         install('function  ',functionsy);
         install('if        ',ifsy);
         install('mod       ',imod);
         install('monitor   ',monitorsy);
         install('not       ',notsy);
         install('null      ',nullsy);
         install('of        ',ofsy);
         install('or        ',orsy);
         install('pri       ',prisy);
         install('procedure ',proceduresy);
         install('process   ',processsy);
         install('program   ',programsy);
         install('record    ',recordsy);
         install('repeat    ',repeatsy);
         install('select    ',selectsy);
         install('terminate ',termsy);
         install('then      ',thensy);
         install('to        ',tosy);
         install('type      ',typesy);
         install('until     ',untilsy);
         install('var       ',varsy);
         install('when      ',whensy);
         install('while     ',whilesy);
         install('at        ',atsy);
         install('offset    ',offsetsy);
         install('address   ',adrsy);
         install('timeout   ',timeoutsy);
         install('forward   ',forwardsy);
         install('entry     ',entrysy);
         install('accept    ',acceptsy);
         install('provides  ',providessy);
         install('replicate ',replicatesy);
		     install('in        ',insy);
		     install('case      ',casesy);
		     install('resource  ',resourcesy);
			   install('guarded   ',guardedsy);
		     install('requeue   ',requeuesy);

         sort
   end;  (* initkeytab *)

     procedure errormsg;

   var
      k : er;

   begin
      writeln(listfile);
      writeln(listfile,' Error diagnostics');
      writeln(listfile);
      for k := erdec to ergrdcall do
         if k in errs  then
            begin
				write(listfile,'E');
				write(listfile,ord(k):1,' - ');
            case k of
               erdec:
						writeln(listfile,' undeclared identifier');
               erdup:
						writeln(listfile,' identifier duplicated');
               erident:
						writeln(listfile,' identifier expected');
               ertyp:
						writeln(listfile,' type error');
					erlparent:
						writeln(listfile,' ''('' expected');
					errparent:
						writeln(listfile,' '')'' expected');
					erlbrack:
						writeln(listfile,' ''['' expected');
					errbrack:
						writeln(listfile,' '']'' expected');
					ercolon:
						writeln(listfile,' '':'' expected');
					ersemi:
						writeln(listfile,' '';'' expected');
					erperiod:
						writeln(listfile,' ''.'' expected');
					ereql:
						writeln(listfile,' ''='' expected');
					erbecomes:
						writeln(listfile,' '':='' expected');
					erprogram:
						writeln(listfile,' ''program'' expected');
					erof:
						writeln(listfile,' ''of'' expected');
					erthen:
						writeln(listfile,' ''then'' expected');
					eruntil:
						writeln(listfile,' ''until'' or ''forever'' expected');
					erdo:
						writeln(listfile,' ''do'' expected');
					erto:
						writeln(listfile,' ''to'' expected');
					erbegin:
						writeln(listfile,' ''begin'' expected');
					erend:
						writeln(listfile,' ''end'' expected');
					erselect:
						writeln(listfile,' ''select'' expected');
					erexport:
						writeln(listfile,' ''export'' expected');
					erreplicate:
						writeln(listfile,' ''replicate'' expected');
               erpar:
						writeln(listfile,' error in parameter list');
               ervarpar:
						writeln(listfile,' must be var parameter');
					erparmatch:
						writeln(listfile,
							' parameter list does not match previous declaration');
               erchar:
						writeln(listfile,' illegal character');
               ersym:
						writeln(listfile,' unexpected symbol');
					erstring:
						writeln(listfile,' string expected');
               erlev:
						writeln(listfile, ' level error');
               ernum:
						writeln(listfile,' number error');
               erassign:
						writeln(listfile,' assignment not permitted');
               ercapsprocdecs:
						writeln(listfile,
                     ' exported monitor/resource procedure(s) not declared');
               erinx:
						writeln(listfile,' must not be var parameter');
					erent:
						writeln(listfile,' malformed entry call');
					ernotinproc:
						writeln(listfile,' not allowed in a process');
					ermap:
						writeln(listfile,' this type must not be mapped');
					ertimetermelse:
						begin
						write(listfile,' ''timeout'' ''terminate'' and ''else''');
						writeln(listfile,' mutually exclusive')
						end;
					ercob:
						writeln(listfile,' multiple cobegins');
					erfordec:
						writeln(listfile,' ''forward'' declaration(s) not resolved');
					erprovdec:
						writeln(listfile,' ''provides'' declaration(s) not resolved');
					ervar:
						writeln(listfile,' variable expected');
					erentmiss:
						writeln(listfile,
							' missing entry or entries declared in "provides"');
					ercasedup:
						writeln(listfile,' case label duplicated');
					erprocinrec:
						writeln(listfile,' processes not allowed in record fields');
					ersetlit:
						writeln(listfile,' invalid set literal');
					ernotprocvar:
						writeln(listfile,' variable is an array, not a process');
					ersub:
						writeln(listfile,' error in array subscript declaration');
					erconst:
						writeln(listfile,' constant expected');
					erentext:
						writeln(listfile,
							' no corresponding "provides" declaration');
					erentmatch:
						writeln(listfile,
							' does not match "provides" declaration');
					ernestacpt:
						writeln(listfile,' illegally nested accept');
					eracptinproc:
						writeln(listfile,' accept not allowed in subprogram');
					ernotingrdproc:
						writeln(listfile,' not allowed in a guarded procedure');
					eronlyingrdproc:
						writeln(listfile,' only allowed in a guarded procedure body');
					ermustbeguarded:
						writeln(listfile,' destination must be guarded procedure');
					eronlyinres:
						writeln(listfile,' only allowed in a resource');
					ergrdcall:
						writeln(listfile,' call not allowed within a resource')
            end;  (* case *)
         end;  (* if k in errs *)
   end;  (* errormsg *)


   procedure endskip;

   (* underline skipped part of input *)

   begin
      while errpos < cc  do
         begin
         write(listfile,'-'); errpos := errpos + 1;
         end;
      skipflag := false
   end;  (* endskip *)


   procedure fatal(n : integer);
   label 99;
   var
      msg : array[1..20] of string;

   begin
      writeln(listfile); errormsg;
      msg[1] := 'identifier'; msg[2] := 'blocks    ';
      msg[3] := 'strings   '; msg[4] := 'arrays    ';
      msg[5] := 'levels    '; msg[6] := 'code      ';
      msg[7] := 'channels  '; msg[8] := 'select    ';
      msg[9] := 'monprocs  '; msg[10] := 'reals     ';
		  msg[11] := 'interrupts'; msg[12] := 'enum type ';
		  msg[13] := 'case      '; msg[14] := 'monitors  ';

	  	writeln(listfile);
		  write(listfile,'FATAL ERROR - ');
      writeln(listfile,'compiler table for ',msg[n],' is too small');
		  success := false;
      goto 99 ; (* terminate compilation *)
      99: writeln;
   end;  (* fatal *)

   procedure nextch;

   (* read next character; process line end *)
   var
    konecRadku: boolean;

      procedure tabtospace;

      (* replace tab character with sufficient spaces *)

      begin
         ch := ' ';
         line[ll] := ch;
         write(listfile, ch);
         while (ll mod tabstop) <> 0 do
            begin
            ll := ll + 1;
				if ll <= llng then
            	line[ll] := ch;
            write(listfile, ch)
            end
      end;  (* tabtospace *)

		procedure fail(n: integer);
    label 99;

		begin
			writeln(listfile); errormsg;
			write(listfile,'FATAL ERROR - ');
			if n = 1 then
				writeln(listfile,'program incomplete')
			else
				writeln(listfile,'input line too long');
			goto 99;
      99:;
		end;  (* fail *)


   begin  (* nextch *)
         if errpos <> 0 then
            begin
            if skipflag  then
               endskip;
            writeln(listfile); errpos := 0
            end;
         if (PositionOfSource < LengthOfSource) then
         Begin
         ch:=Source[PositionOfSource];
         inc(PositionOfSource);
         write(listfile, ch);
         End
          else
           begin
              Done:=true;
           end;
         {writeln(listfile);}
   end; (*nextch*)

   procedure error(n : er);


   begin
      if errpos = 0  then
         write(listfile,'__________');
      if cc > errpos   then
         begin
			if n = erchar then
         	write(listfile,' ':cc-errpos,'^',ord(n) : 2)
			else
				write(listfile,' ':cc-errpos-1,'^',ord(n):2,' ');
         errpos := cc + 3; errs := errs + [n]
         end
   end;  (* error *)

 	procedure tolower(var alfavar: alfa);

	(* convert to lower case *)

	var
		index: 1..alng;

	begin
		for index := 1 to alng do
			if alfavar[index] in ['A'..'Z'] then
				alfavar[index] := chr(ord(alfavar[index])+(ord('a') - ord('A')))
	end;  (* tolower *)




(*-----------------------------------------------------insymbol-*)


   procedure insymbol;

   (* read next symbol (lexical analysis) *)

   label
      1,2,3;

	const
		maxdigits = 80;  (* maximum digits in real constant before point or e *)

   var
      i,j,k,l : integer;
		digitbuff: array[1..maxdigits] of char;


	procedure collectint;

	begin
		l := 0;
     	repeat
        	if inum > (intmax div 10) then
         	numerror := true
        	else
           	begin
           	inum := inum*10;
				l := l + 1;
				if l > maxdigits then
					numerror := true
				else
        			digit := ord(digitbuff[l]) - ord('0');
        		if digit > (intmax - inum) then
           		numerror := true
        		else
        			inum := inum + digit
        		end
     until (l = k) or numerror
	end;  (* collectint *)


	procedure collectreal;

	(* collect whole number part from digit buffer *)

	var
		l: integer;

	begin
		l := 0;
		repeat
			l := l + 1;
			if rnum > (realmax/10.0) then
				e := e + 1
			else
				begin
				rnum := rnum*10.0;
				if l <= maxdigits then
					begin
					digit := ord(digitbuff[l]) - ord('0');
					if digit <= (realmax - rnum) then
						rnum := rnum + digit
					end
				end
		until l = k;
		k := k - e
	end;  (* collectreal *)





	procedure readscale(var numerror: boolean);

	var
		s, sign, digit: integer;

	begin
		nextch;
		sign := 1; s := 0;
		if ch = '+' then
			nextch
		else
			if ch = '-' then
				begin
				nextch; sign := -1
				end;
		if not (ch in ['0'..'9']) then
			numerror := true
		else
			repeat
				if s > (intmax div 10) then
					numerror := true
				else
					begin
					s := 10*s;
					digit :=  ord(ch) - ord('0');
					if digit > (intmax - s) then
						numerror := true
					else
						s := s + digit
					end;
				nextch
			until not (ch in ['0'..'9']);
		if numerror then
			e := 0
		else
			e := s*sign + e
	end;  (* readscale *)


	procedure adjustscale(var numerror: boolean);

	var
		s: integer;
		d, t: real;

	begin
	 	if (k + e) > emax then
			numerror := true
		else
			begin
			while e < emin do
				begin
				rnum := rnum/10.0;
				e := e + 1
				end;
			s := abs(e); t := 1.0; d := 10.0;
			repeat
				while not odd(s) do
					begin
					s := s div 2; d := sqr(d)
					end;
				s := s - 1; t := d*t
			until s = 0;
			if e >= 0 then
				if rnum > (realmax/10.0) then
					numerror := true
				else
					rnum := rnum *t
			else
				rnum := rnum/t
			end
	end;  (* adjustscale *)



   begin  (* Insymbol *)
		lineold := linenew;
		linenew := linenum;
      1:
      while ch=' ' do nextch;
      if ch in legalchars then
      case ch of
				'A','B','C','D','E','F','G','H','I',
				'J','K','L','M','N','O','P','Q','R',
				'S','T','U','V','W','X','Y','Z',
         'a','b','c','d','e','f','g','h','i',
         'j','k','l','m','n','o','p','q','r',
         's','t','u','v','w','x','y','z':
            begin
               (*identifier or wordsymbol*)k := 0;id := '          ';
               repeat
                  if k < alng
                  then
                     begin
                        k :=k+1;id[k] := ch
                     end;
                  nextch
               until not (ch in ['A'..'Z','a'..'z','0'..'9']);
					tolower(id);
               i := 1;j := nkw; (*binary search*)
               repeat
                  k := (i+j) div 2;
                  if id <= keywords[k].key
                  then j := k-1;
                  if id >= keywords[k].key
                  then i := k+1
               until i > j;
               if i-1 > j
               then sy := keywords[k].ksy
               else sy := ident
            end;

         '0','1','2','3','4','5','6','7','8','9':
            begin
               (*number*)
               k := 0; inum := 0; sy := intcon;
               numerror := false;
					repeat
						k := k + 1;
						if k <= maxdigits then
							digitbuff[k] := ch;
						nextch
					until not (ch in['0'..'9']);
					if not (ch in ['.','e','E']) then
						begin (* integer *)
						collectint;
               	if numerror then
               		inum := 0;
               	if ch ='#' then
                  	begin  (* based integer *)
                  	nextch;
                  	if (inum in [2, 8, 16]) then
								base := inum
							else
								begin
								base := 16;
								numerror := true
								end;
							inum := 0; negative := false;
                  	repeat
                     	if negative then
                        	numerror := true
                     	else
                        	begin
                        	if inum > (intmax div base) then
                           	begin
										if inum <= (intmax div (base div 2)) then
                           		negative := true
										else
											numerror := true;
                           	inum := inum mod (intmax div base + 1)
                           	end;
                        	inum := inum*base;
                        	if ch in ['0'..'9'] then
                           	digit := ord(ch) - ord('0')
                        	else
										if ch in ['A'..'Z'] then
											digit := ord(ch) - ord('A') + 10
										else
											if ch in ['a'..'z'] then
                           			digit := ord(ch) - ord('a') + 10
											else
												numerror := true;
                        	if digit >= base then
                           	numerror := true
                        	else
                           	inum := inum + digit
                        	end;
                     	nextch
                  	until not (ch in ['0'..'9','A'..'Z','a'..'z']);
                  	if negative then
								if inum = 0 then
									numerror := true
								else
                     		inum := (-intmax + inum) - 1;
                  	if numerror then
                     	inum := 0
                  	end  (* based integer *)
						end  (* integer *)
					else
						if ch = '.' then
							begin  (* fractional part *)
							nextch;
							if ch = '.' then
								begin
								ch := ':';
								collectint
								end
							else
								begin
								sy := realcon; rnum := 0.0; e := 0;
								collectreal;
								if ch in ['0'..'9'] then
									while ch in ['0'..'9'] do
										begin
										if rnum <= (realmax/10.0) then
											begin
											e := e - 1;
											rnum := 10.0*rnum;
											digit :=  ord(ch) - ord('0');
											if digit <= (realmax - rnum) then
												rnum := rnum + digit
											end;
										nextch
										end
								else
									numerror := true;
								if ch in ['e','E'] then readscale(numerror);
								if e <> 0 then adjustscale(numerror)
								end
							end  (* fractional part *)
						else
							if ch in ['e','E'] then
								begin
								sy := realcon; rnum := inum; e := 0;
								collectreal;
								readscale(numerror);
								if e <> 0 then adjustscale(numerror)
								end;
				if numerror then error(ernum)
            end;  (* number *)

         ':':
            begin
               nextch;
               if ch = '='
               then
                  begin
                     sy := becomes;nextch
                  end
               else sy := colon
            end;

         '<' :
            begin
               nextch;
               if ch = '='
               then
                  begin
                     sy := leq; nextch
                  end
               else
               if ch = '>'
               then
                  begin
                     sy := neq; nextch
                  end
               else
                  sy := lss
            end;

         '>' :
            begin
               nextch;
               if ch = '='
               then
                  begin
                     sy := geq; nextch
                  end
               else
                  sy := gtr
            end;

         '.' :
            begin
               nextch;
               if ch = '.'
               then
                  begin
                     sy := colon; nextch
                  end
               else sy := period
            end;

         '''':
            begin
               k := 0;
               2:  nextch;
               if ch = ''''
               then
                  begin
                     nextch;
                     if ch <> ''''
                     then goto 3
                  end;
               if sx+k = smax
               then fatal(3);
               stab[sx+k] := ch; k := k+1;
               if cc = 1
               then
                  begin
                     (*end of line*) k:=0;
                  end
               else goto 2;
               3:
               if k = 1
               then
                  begin
                     sy := charcon;inum := ord(stab[sx])
                  end
               else
               if k = 0
               then
                  begin
                     error(erstring); sy := charcon; inum :=0
                  end
               else
                  begin
                     sy := string1; inum := sx; sleng := k; sx := sx+k
                  end
            end;


         '(','{':
            begin
               if ch='(' then
                  nextch;
               if  not (ch in ['*','{']) then
                    sy := lparent
               else
                  begin
                     (*comment*) nextch;
                     repeat
                        while not (ch in['*','}']) do
                           nextch;
                        if ch='*' then
                           nextch
                     until ch in [')','}'];
                     nextch; goto 1
                  end
            end;

			'}':
				begin
				sy := rbrace;
				nextch
				end;

         '=':
       begin
          nextch;
          if ch='>' then
        begin
        sy:=arrow;
        nextch
             end
          else
        sy:=eql
       end;

         '+','-','*','/',')',',','[',']',';' ,'?','!','%':
            begin
               sy := sps[ch]; nextch
            end;

      end   (* case *)
	else
            begin   (* not legal character *)
              { error(erchar);} nextch; goto 1
            end
   end; (* insymbol *)

  (*-----------------------------------------------------------------------enter---*)

   procedure enter(x0: alfa; x1: objekt; x2: types; x3: integer);

   begin
      t := t+1; (*enter standard identifiers*)
      with tab[t] do
         begin
         name := x0; link := t-1; obj := x1;
         typ := x2; ref := 0; normal := true;
         lev := 0; taddr := x3;  auxref := 0
         end
   end; (* enter *)

   procedure enterarray(tp: types; l,h: integer);

   begin
      if l>h then
         error(ersub);
      if (abs(l) > xmax) or (abs(h) > xmax)  then
         begin
         error(ersub); l := 0; h := 0;
         end;
      if a = amax  then
         fatal(4)
      else
         begin
         a := a+1;
         with atab[a] do
            begin
            inxtyp := tp; low := l; high := h
            end
         end
   end; (* enterarray *)

   procedure enterblock;

   begin
      if b = bmax  then
         fatal(2)
      else
         begin
         b := b+1;
			with btab[b] do
				begin
				last := 0; lastpar := 0 ;
         	tabptr := t
         	end
			end
   end;  (* enterblock *)


	procedure enterreal(x: real);


	begin
		if r = (rmax - 1) then
			fatal(10)
		else
			begin
			rconst[r+1] := x; realindex := 1;
			while rconst[realindex] <> x do
				realindex := + 1;
			if realindex > r then
				r := realindex
			end
	end;  (* enterreal *)



   procedure emit0typed(fop: opcode; tp: types);

   begin
      if lc=cmax then
         fatal(6);
      with code[lc] do
         begin
         f := fop;
         instyp := tp;
			line := lineold
         end;
      lc := lc + 1
   end;  (* emit0typed *)


   procedure emit0(fop: opcode);

   begin
      emit0typed(fop, notyp)
   end;  (* emit0 *)


   procedure emit1typed(fop: opcode; b: integer; tp: types);

   begin
      if lc=cmax then
         fatal(6);
      with code[lc] do
         begin
         f := fop;
         y := b ;
         instyp := tp;
			line := lineold
         end;
      lc := lc+1
   end;  (* emit1typed *)



   procedure emit1(fop: opcode; b: integer);

   begin
      emit1typed(fop,b,notyp)
   end;  (* emit1 *)


   procedure emit2typed(fop: opcode; a,b: integer; tp: types);

   begin
      if lc = cmax then
         fatal(6);
      with code[lc] do
         begin
         f := fop;
         x := a;
         y := b ;
         instyp := tp;
			line := lineold
         end;
      lc := lc+1
   end;  (* emit2 *)



   procedure emit2(fop: opcode; a,b: integer);

   begin
      emit2typed(fop,a,b,notyp)
   end;  (* emit2 *)


   procedure initmons;

   (* initialise monitor procedure table *)

   var i: 1..maxcapsprocs;

   begin
      ncapsprocs := 0;
      curcaps := 0;
		inguardedproc := false;
      for i := 1 to maxcapsprocs do
         with capsproctab[i] do
            begin
            name := '          ';
            foundec := false
            end
   end;  (* initmons *)




   procedure block(fsys: symset; lobj: objekt;
         prt: integer; level: integer);

   type
      conrec=
      record
         case tp:types of
         	ints,
				bools,
				chars:	(i: integer);
				enums:	(ordval: integer; ref: index);
				reals:	(r: real)
      end;

   var
      dx, prb, ttt, x:integer;            (* data allocation index *)
      entoffset: integer;
		codelevel: integer;
        debug : integer;

      procedure skip(fsys: symset; n: er);

      begin
         error(n);skipflag:=true;
         while not (sy in fsys) do insymbol;
         if skipflag   then
            endskip
      end;  (* skip *)

      procedure test(s1,s2: symset; n: er);

      begin
         if not(sy in s1)  then
            skip(s1+s2,n)
      end;  (* test *)

      procedure testsemicolon;

      begin
         if sy=semicolon then
            insymbol
         else
            error(ersemi);
         test([ident]+blockbegsys,fsys,ersemi)
      end;  (* testsemicolon *)



		function searchblock(k: integer; id: alfa): integer;

		(* search a single static level for an identifier *)
		(* search symbol table backwards from k *)

		begin
			tab[0].name := id;
			while tab[k].name <> id do
				k := tab[k].link;
			searchblock := k
		end;  (* searchblock *)




      procedure enter(id: alfa; k: objekt);

		(* enter new identifier into symbol table *)

      var
         j,l:integer;

      begin
         if t = tmax   then
            fatal(1)
         else
            begin
            l:=btab[display[level]].last;
				if id = '          ' then
					j := 0
				else
					j := searchblock(l,id);
            if j <> 0 then
               error(erdup)
            else
               begin
               t:=t+1;
               with tab[t] do
                  begin
                  name:=id;link:=l;
                  obj:=k;typ:=notyp;ref:=0;lev:=level;taddr:=0;
						auxref := 0
                  end;
               btab[display[level]].last:=t
               end   (* j=0 *)
            end
      end;  (* enter *)




      function find(id: alfa): integer;

      (* find id in table or return 0 if not present *)

      var
         i,j:integer;

      begin
         i:=level;
         repeat
            j:=searchblock(btab[display[i]].last,id);
            i:=i-1;
         until (i<0) or (j<>0);
         find:=j;
      end;  (* find *)



      function loc(id: alfa): integer;

		(* find with an error message *)

      var
         j: integer;

      begin
         j := find(id);
         if j=0  then
            error(erdec);
         loc:=j;
      end;  (* loc *)



      procedure constant(fsys: symset; var c: conrec);

      var
         x,sign: integer;
			hasasign: boolean;

      begin
         c.tp := notyp; c.i := 0;
			hasasign := false;
         test(constbegsys,fsys,ersym);
         if sy in constbegsys  then
            begin
            if sy = charcon   then
               begin
               c.tp := chars; c.i := inum; insymbol
               end
            else
               begin
               sign := 1;
               if sy in [plus,minus] then
                  begin
						hasasign := true;
                  if sy = minus   then
                     sign := -1;
                  insymbol
                  end;
               if sy = ident then
                  begin
                  x := loc(id);
                  if x <> 0 then
                     if tab[x].obj <> konstant  then
                        error(erconst)
                     else
                        begin
                        c.tp := tab[x].typ;
								if hasasign and not(c.tp in [ints,reals]) then
									error(ertyp);
								case c.tp of
									ints:		c.i := sign*tab[x].taddr;
									notyp,
									chars,
									bools:	c.i := tab[x].taddr;
									enums:	begin
												c.ordval := tab[x].taddr;
												c.ref := tab[x].auxref
												end;
									reals:	c.r := sign*rconst[tab[x].taddr];
								end  (* case c.tp of *)
                        end
							else
								begin  (* x = 0 *)
								c.tp := notyp;
								c.i := 0
								end;
                  insymbol
                  end   (* sy was ident *)
               else
                  if sy = intcon  then
                     begin
                     c.tp := ints; c.i := sign*inum; insymbol
                     end
                  else
							if sy = realcon then
								begin
								c.tp := reals;
								c.r := sign * rnum;
								insymbol
								end
							else
                     	skip(fsys,ersym)
               end;
            test(fsys,[],ersym);
            end
      end;  (* constant *)


      procedure entervariable;

      begin
         if sy=ident then
            begin
            enter(id,variable);insymbol
            end
         else
            error(erident)
      end;  (* entervariable *)

    procedure align(var dx: integer);

      (* align objekt to boundary required by target machine *)

      var
            rem: integer;

      begin
         rem := dx mod objalign;
         if rem > 0 then
            dx := dx + (objalign - rem)
      end;  (* align *)



		procedure alloc(sz: integer; var dx, taddr: integer);

		(* allocate space for variable *)

		begin
			if pushdown then
				begin
				dx := dx + sz;
				align(dx);
				taddr := dx
				end
			else
				begin
				align(dx);
				taddr := dx;
				dx := dx + sz
				end
		end;  (* alloc *)




		procedure enterint(i, sz: integer; var dx, taddr: integer);

        var
            debug : integer;

		(* enter mapped ipc type into interrupt map table *)

		begin
         if int=intermax then fatal(11);
         int := int + 1;
         with intab[int] do
             begin
				 tabref := i;
             with tab[i] do
					begin
              	tp := typ;
              	rf := ref;
					lv := lev;
              	vector := taddr;
					off := dx;
                    debug := taddr;
					alloc(sz,dx,debug);
                    taddr := debug
					end;
				if pushdown then
            	off := dx
             end;  (* with *)
		end;  (* enterint *)


      function contains(targetset: typset; tp: types; rf: index): boolean;

      (* returns true if any component of objekt is in target set *)

      var
         found: boolean;
         j: integer;

      begin  (* contains *)
         if tp in targetset then
            contains:=true
         else
            if rf=0 then
               contains:=false
            else
               if tp=arrays then
                  contains:=contains(targetset,atab[rf].eltyp,atab[rf].elref)
               else
                  if tp=records then
                     begin
                     j:=btab[rf].last;
                     found:=false;
                     while not found and (j <> 0) do
                        begin
                        found:=contains(targetset,tab[j].typ,tab[j].ref);
                        j:=tab[j].link
                        end;  (* while *)
                     contains:=found
                     end  (* record *)
                  else
                     contains := false
      end;  (* contains *)


    procedure  getmapping(possibles: symset);

      (* get mapping information if any *)

      var
            ad: conrec;

      begin
         if sy=atsy then
            begin
            insymbol;
            if sy in possibles then
               begin
               if not (sy in constbegsys) then
                  insymbol;
               tab[t].obj := address;
               constant(fsys+[comma,colon],ad);
               if ad.tp=ints then
                  tab[t].taddr := ad.i
               else
                  begin
                  error(ertyp); tab[t].taddr := 0
                  end
               end
            else
               error(ersym)
            end  (* sy was atsy *)
      end;  (* getmapping *)





	procedure internalname(var internalnum: integer;
	                       var namestring: alfa);
	var
		temp: integer;
		index: integer;

	begin
		namestring := '$         ';
		internalnum := internalnum + 1;
		temp := internalnum;
		index := 2;
		while temp <> 0 do
			begin
			namestring[index] := chr((temp mod 10) + ord('0'));
			temp := temp div 10;
			if index < 10 then
				index := index + 1
			end
	end;  (* internalname *)





      procedure typ(fsys:symset; var tp : types; var rf,sz : integer);

      var
         eltp: types;
         elrf, x : integer;
         elsz, offset, t0, t1: integer;

         procedure arraytyp(var aref,arsz: integer);

         var
            eltp: types;
            low, high: conrec;
            irf,elrf,elsz: integer;

         begin  (* arraytyp *)
            constant([colon,rbrack,ofsy]+fsys,low);
				if low.tp = reals then
					error(ertyp);
            if sy = colon  then
               insymbol
            else
					skip(fsys+constbegsys+[rbrack,colon],erperiod);
            constant([rbrack,comma,ofsy]+fsys,high);
            if high.tp <> low.tp then
               begin
               error(ertyp); high.i := low.i
               end
				else
					if low.tp = enums then
						if low.ref <> high.ref then
							begin
							error(ertyp);
							irf := 0
							end
						else
							irf := low.ref
					else
						irf := 0;
				if low.tp = reals then
					enterarray(notyp,0,0)
				else
            	enterarray(low.tp,low.i,high.i);
				aref := a;
            if sy = comma then
               begin
               insymbol; eltp := arrays; arraytyp(elrf,elsz)
               end
            else
               begin
               if sy = rbrack then
                    insymbol
               else
                  error(errbrack);
               if sy = ofsy then
                  insymbol
               else
                  error(erof);
               typ(fsys,eltp,elrf,elsz)
               end;
            with atab[aref] do
               begin
               arsz := (high-low+1)*elsz; size := arsz;
               eltyp := eltp; elref := elrf; elsize := elsz ;
					inxref := irf
               end;
         end; (* arraytyp *)


			procedure enumtyp;

			(* parse enumeration type declaration *)

			var
				ordval: integer;

			begin
				if et = etmax then
					fatal(12);
				et := et + 1;
				ordval := 0;
				insymbol;
				while sy = ident do
					begin
					enter(id,konstant);
					with tab[t] do
						begin
						typ := enums;
						ref := 0;;
						auxref := et;
						taddr := ordval
						end;  (* with *)
					ordval := ordval + 1;
					insymbol;
					if sy = comma then insymbol
					end;  (* while *)
				with bounds[et] do
					begin
					lower := 0;
					upper := ordval - 1
					end;
				if sy = rparent then insymbol else error(errparent)
			end;  (* enumtyp *)


      begin   (* typ *)
         tp := notyp; rf := 0; sz := 0;
         test(typebegsys,fsys,ersym);
         if sy in typebegsys then
            begin
            case sy of

            ident:
                  begin
                  x := loc(id);
                  if x <> 0   then
                     with tab[x] do
                        if obj <> type1  then
                           error(ertyp)
                        else
                           begin
                           tp := typ;
									if tp = enums then
										rf := auxref
									else
                           	rf := ref;
                           if tp = procs then
                              sz := procsize
                           else
                              sz := taddr;
                           if tp = notyp  then
                              error(ertyp)
                           end;
                  insymbol
                  end;   (* sy was ident *)
            arraysy:
                  begin
                  insymbol;
                  if sy = lbrack  then
                        insymbol
                  else
                     error(erlbrack);
                  tp := arrays; arraytyp(rf,sz)
                  end;   (* sy was arraysy *)
            recordsy:
                  begin
                  insymbol;
                  enterblock;
                  tp := records; rf := b;
                  if level = lmax then fatal(5);
                  level := level + 1; display[level] := b; offset := 0;
                  while not (sy in fsys -
                               [semicolon,comma,ident] + [endsy]) do
                     begin  (* field section *)
                     if sy=ident  then
                        begin
                        t0 := t; entervariable;
                        getmapping([offsetsy]);
                        while sy=comma do
                           begin
                           insymbol; entervariable;
                           getmapping([offsetsy])
                           end;
                        if sy=colon then insymbol else error(ercolon);
                        t1 := t;
                        typ(fsys+[semicolon,endsy,comma,ident],
                                                      eltp,elrf,elsz);
								if contains([procs],eltp,elrf) then
									error(erprocinrec);
                        while t0 < t1 do
                           begin
                           t0 := t0+1;
                           with tab[t0] do
                              begin
                              typ := eltp;
                              ref := elrf; normal := true;
                              if obj = variable then
                                 begin
											align(offset);
                                 taddr := offset;
											offset := offset+elsz
                                 end
                              else
                                 offset := taddr + elsz;
										obj := variable
                              end
                           end
                        end;  (* sy=ident *)
                     if sy <> endsy then
                        begin
                        if sy=semicolon then
                           insymbol
                        else
                           begin
                           error(ersemi);
                           if sy=comma then insymbol
                           end;
                        test([ident,endsy,semicolon],fsys,ersym);
                        end
                     end; (* field section *)
						align(offset);
                  btab[rf].vsize := offset; sz := offset;
                  btab[rf].psize := 0;
                  insymbol; level := level-1
                  end;  (* records *)
            channelsy:
                  begin  (* channel *)
                  insymbol;
                  if chan = chanmax then
                     fatal(7)
                  else
                     begin
                     chan := chan + 1;
                     tp := channels; rf := chan; sz := chansize;
                     if sy=ofsy then insymbol else error(erof);
                     typ(fsys+[semicolon], eltp, elrf, elsz);
                     with chantab[chan] do
                        begin
                        eltyp := eltp;
                        elref := elrf;
                        elsize := elsz
                        end  (* with *)
                     end
                  end;  (* channel *)
				lparent:
						begin
						enumtyp;
						tp := enums;
						rf := et;
						sz := intsize
						end  (* enum type *)
            end;  (* case sy of *)
            test(fsys,[],ersym);
            end   (* sy was in typebegsys *)
      end;  (* typ *)


       procedure parameterlist(isentry: boolean; var dx: integer);

       (* formal parameter list *)

      var
         tp : types;
         rf,sz,x,t0 : integer;
         valpar : boolean;
         debug : integer;

      begin
         entoffset := entrysize;
         insymbol; tp := notyp; rf := 0; sz := 0;
         test([ident,varsy],fsys+[rparent],erpar);
         while sy in [ident,varsy] do
            begin
            if sy <> varsy  then
               valpar := true
            else
               begin
               insymbol; valpar := false
               end;
            t0 := t; entervariable;
            while sy = comma do
               begin
               insymbol; entervariable;
               end;
            if sy = colon then
               begin
               insymbol;
               if sy <> ident  then
                  error(erident)
               else
                  begin
                  x := loc(id); insymbol;
                  if x <> 0 then
                     with tab[x] do
                        if obj <> type1  then
                           error(ertyp)
                        else
                           begin
                           tp := typ;
									if tp = enums then
										rf := auxref
									else
										rf := ref;
                           if valpar  then
                               begin
                              if contains([semafors,channels,
                                                condvars],typ,ref) then
                                 error(ervarpar);
                                sz := taddr
                              end
                           else
                              sz := intsize
                           end;
                  end;
               test([semicolon,rparent],[comma,ident]+fsys,ersym)
               end   (* sy was colon *)
            else
               error(ercolon);
            while t0 < t do
               begin
               t0 := t0 + 1;
               with tab[t0] do
                  begin
                  typ := tp;
						if tp = enums then
							begin
							auxref := rf;
							ref := 0
							end
						else
							begin
							auxref := 0;
							ref := rf
							end;
                  normal := valpar;
                  if isentry then
                     lev := level - 1;
                  debug := taddr;
                  alloc(sz,dx,debug);
                  taddr := debug;
                  end
               end;
            if sy <> rparent  then
               begin
               if sy = semicolon then
                  insymbol
               else
                  error(ersemi);
               test([ident,varsy],[rparent]+fsys,ersym);
               end
            end (* while sy in [ident,varsy] *);
         if sy = rparent then
            begin
            insymbol;
            test([semicolon,colon,providessy,whensy],fsys,ersym);
            end
         else
            error(errparent)
      end;  (* parameterlist *)


      procedure parametercheck(i: integer);

      (* check consistency of formal entry parameter declarations *)
		(* used by accept and when "provides" has been used *)

      var
         valpar, perror: boolean;
         lastp, cp, k, t0, rf: integer;
         tp: types;


		procedure checkident;

		begin
			cp := cp + 1;
			if id <> tab[cp].name then perror := true;
			insymbol
		end;  (* checkident *)



      begin
         lastp := btab[tab[i].ref].lastpar; cp := i;
         if sy = lparent then
            begin
            perror := false;
            insymbol;
            while sy in [ident, varsy] do
               begin
               if sy=varsy then
                  begin
                  valpar := false;
                  insymbol
                  end
               else
                  valpar := true;
               t0 := cp;
					checkident;
					while sy = comma do
						begin
						insymbol;
						checkident
						end;
               if sy = colon then insymbol else error(ercolon);
               if sy = ident then
                  begin
                  k := find(id);
                  if tab[k].obj <> type1 then error(ersym);
                  tp := tab[k].typ;
						if tp = enums then
							rf := tab[k].auxref
						else
							rf := tab[k].ref;
                  insymbol
                  end
               else
                  error(erident);
               while t0 < cp do
                  begin
                  t0 := t0 + 1;
                  with tab[t0] do
							if (valpar <> normal) or (tp <> typ) then
								perror := true
							else
								if typ = enums then
									begin
									if rf <> auxref then
										perror := true
									end
								else
                     		if rf <> ref then
                        		perror := true
                  end;
               if sy = semicolon then insymbol
               end;  (* while sy in [ident, varsy] *)
            if perror then error(erparmatch);
            if sy = rparent then
					insymbol
				else
					skip(fsys+[semicolon,dosy],erlparent)
            end;
         if cp <> lastp then error(erpar)
      end;  (* parametercheck *)


      procedure entrycheck(i: integer);

      (* check consistency of entry declarations *)
		(* used when "provides" was used *)

      var
         k, prb: integer;
         missing: boolean;
			ad: conrec;

      begin
         prb := tab[i].ref;
         k := btab[prb].last;
         while k <> 0 do
            begin
            if tab[k].typ = entrys then
               tab[k].auxref := 1;
            k := tab[k].link
            end;
         while sy = entrysy do
            begin
            insymbol;
            if sy <> ident then
               skip([semicolon,entrysy]+fsys,erident)
            else
               begin  (* sy is ident *)
               k := searchblock(btab[prb].last,id);
               insymbol;
               if (k = 0) or (tab[k].typ <> entrys) then
                  skip([semicolon,entrysy]+fsys,erentext)
					else
                  begin  (* typ = entrys *)
                  tab[k].auxref := 0;
                  parametercheck(k);
						if sy = atsy then
							begin
							insymbol;
							constant(fsys+[semicolon,endsy],ad);
							if tab[k].obj <> address then
								error(erentmatch)
							else
								if ad.i <> tab[k].taddr then
									error(erentmatch)
							end
						else
								if tab[k].obj = address then
									error(erentmatch)
                  end  (* typ = entrys *)
               end;  (* sy is ident *)
               if sy = semicolon then insymbol else error(ersemi)
            end;  (* while sy = entrysy *)
         missing := false;
			k := btab[prb].last;
			while (k <> 0) and not missing do
				begin
				if (tab[k].typ = entrys) and (tab[k].auxref <> 0) then
					missing := true;
				k := tab[k].link
				end;
         if missing then
            error(erentmiss)
      end;  (* entrycheck *)

procedure constantdeclaration;

      var
         c : conrec;

      begin
         insymbol;
         test([ident],blockbegsys,erident);
         while sy = ident do
            begin
            enter(id,konstant); insymbol;
            if sy = eql then
               insymbol
            else
               error(ereql);
            constant([semicolon, comma, ident]+fsys,c);
            tab[t].typ := c.tp; tab[t].ref :=0;
				if c.tp = enums then
					begin
					tab[t].auxref := c.ref;
					tab[t].taddr := c.ordval
					end
				else
					if c.tp = reals then
						begin
						enterreal(c.r);
						tab[t].taddr := realindex
						end
					else
            		tab[t].taddr :=c.i;
            testsemicolon
            end
      end;  (* constantdeclaration *)

		procedure testlevel(tp: types; rf: index);

		(* test for level error in type *)

		begin
         if contains([semafors,channels,procs],tp,rf) and (level <> 1) then
               error(erlev)
         else
            if contains([condvars],tp,rf) then
					if curcaps = 0 then
						begin  (* type declarations can be in main program *)
						if level <> 1 then
							error(erlev)
						end
					else
						if (level <> 2) or (tab[curcaps].typ <> monvars) then
							error(erlev)
		end;   (* testleveL *)



      procedure typedeclaration;

      var
         tp:types;
         rf,sz,t1:integer;

      begin
         insymbol;
         test([ident],blockbegsys,erident);
         while sy=ident do
            begin
				enter(id,type1);
				t1 := t;
				insymbol;
            if sy=eql then
               insymbol
            else
               error(ereql);
            typ([semicolon, comma, ident]+fsys, tp, rf, sz);
				testlevel(tp,rf);
            with tab[t1] do
               begin
               typ:=tp;
					if tp = enums then
						begin
						auxref := rf;
						ref := 0
						end
					else
						begin
						auxref := 0;
						ref := rf
						end;
               taddr:=sz ;
					if tp = procs then
						normal := true
               end;
            testsemicolon
            end
      end;  (* typedeclaration *)




      procedure variabledeclaration(var dx: integer);

      var
         t0,t1,rf,sz:integer;
         tp:types;
         debug : integer;

      begin
         insymbol;
			test([ident],[colon,semicolon],erident);
         while sy=ident do
            begin
            t0:=t; entervariable;
            getmapping(constbegsys+[adrsy]);
            while sy=comma do
               begin
               insymbol; entervariable;
               getmapping(constbegsys+[adrsy])
               end;
            if sy=colon then
               insymbol
            else
               error(ercolon);
            t1:=t;
				typ([semicolon,comma,ident]+fsys,tp,rf,sz);
				testlevel(tp,rf);
				if contains([condvars],tp,rf) and (curcaps = 0) then
					error(erlev);
            while t0<t1 do
               begin
               t0:=t0+1;
               with tab[t0] do
                  begin
                  typ:=tp;
						if tp = enums then
							begin
							auxref := rf;
							ref := 0
							end
						else
							begin
							auxref := 0;
							ref := rf
							end;
                  normal := true;
                  if obj <> address then
                     begin
                     if (curcaps <> 0) and (level = 2) then
								lev := 1
							else
								lev:=level;
                            debug := taddr;
							alloc(sz,dx,debug);
                            taddr := debug
                     end
                  else
                     if typ in [semafors, channels] then
                            begin
                                debug := taddr;
								enterint(t0,sz,dx,debug);
                                taddr := debug
                            end
							else
								if contains(ipctyps+[procs],tp,rf) then
									error(ermap)
                  end
               end;
            testsemicolon
            end
      end;  (* variabledeclaration *)

      function isexported: boolean;

      (* returns true if procedure is exportable from monitor *)

      var
         found: boolean;
         i: 0..maxcapsprocs;

      begin
         found := false;
         if (curcaps <> 0) then
            begin
            i := 0;
            while (i < ncapsprocs) and not found do
               begin
               i := i + 1;
               if capsproctab[i].name = id then
                  begin
                  found := true;
                  capsproctab[i].foundec := true
                  end
               end
            end;
         isexported := found
		end;  (* isexported *)

      procedure procdeclaration;

      var
         lobj: objekt;
         i, prt : integer;



      begin
			if sy = functionsy then
				lobj := funktion
			else
				lobj := prozedure;
         insymbol;
         if sy <> ident then
            begin
            error(erident);
				id := '          ';
				i := 0
            end
			else
         	i := searchblock(btab[display[level]].last,id);
         if (i = 0) or tab[i].normal then
            begin  (* no pending forward declaration *)
            if isexported then
					begin
					if level <> 2 then error(erlev);
               enter(id, monproc)
					end
            else
               enter(id,lobj);
            tab[t].normal := true;
            tab[t].typ := notyp;
            prt := t
            end  (* no pending forward declaration *)
         else
				begin   (* pending forward declaration *)
				if tab[i].obj <> lobj then
					error(erdup);
            prt := i
				end;  (* pending forward declared *)
         insymbol; block([semicolon]+fsys,lobj,prt,level+1);
         if tab[prt].normal then
            if lobj = funktion then
               emit0typed(retfun,tab[prt].typ)
            else
               emit0(retproc);
         if sy=semicolon then
            insymbol
         else
            error(ersemi)
      end;  (* proceduredeclaration *)


      procedure processdeclaration;

      var
         i, prt: integer;
         anon, notforward, nestedproc: boolean;
         debug : integer;

      begin
			nestedproc := inprocessdec;
			inprocessdec := true;
         anon := false;
         notforward := true;
			if nestedproc then
				error(ernotinproc)
			else
         	if level <> 1 then error(erlev);
         insymbol;
         if sy = typesy then
            insymbol
         else
            anon := true;
         if sy <> ident then
            begin
            error(erident);
            id := '          '
            end;
         if id = '          ' then
            i := 0
         else
            i := find(id);
         if (i <> 0) and (tab[i].lev = level) then
            if tab[i].typ <> procs then
               error(erdup)
            else
               begin  (* id seen before *)
               notforward := false;
               if tab[i].obj = type1 then
                  prt := i
               else
                  prt := btab[tab[i].ref].tabptr;
               if tab[prt].normal or
                  ((tab[i].obj = type1) and anon) or
                  ((tab[i].obj = variable) and not anon) then error(erdup)
               end  (* id seen before *)
         else
            begin  (* id not seen before *)
            if anon then
               begin
            	enter(id,variable);
            	with tab[t] do
               	begin
               	typ := procs; ref := b + 1;
               	normal := true; lev := level;
                        debug := taddr;
						alloc(intsize,dx,debug);
                        taddr := debug;
               	end;
					(* enter cannot be used - $ not unique *)
               if t = tmax then fatal(1);
               t := t + 1;
					with tab[t] do
						begin
               	name := '$         ';
               	obj := type1;
						link := btab[display[level]].last;
						btab[display[level]].last := t
						end
               end  (* if anon *)
            else
               enter(id,type1);
            tab[t].normal := true; tab[t].lev := level;
            tab[t].typ := procs;
            prt := t
            end;
         insymbol;
         block([semicolon]+fsys,prozedure,prt,level+1);
         if tab[prt].normal then
            emit2(retproc,1,0);
         if sy = semicolon then insymbol else error(ersemi);
			inprocessdec := nestedproc
      end;  (* processdeclaration *)




  (*--------------------------------------------------------------------*)

   procedure expression(fsys: symset; var x: item); forward;


            procedure selector(fsys: symset; var v: item);

            var
               x: item; a, j: integer;

            begin  (* Selector *)
            repeat
               if sy=period then
                  begin  (* record field or process entry *)
                  insymbol;
                  if sy <> ident then
                       error(erident)
                  else
                     begin
                     if not (v.typ in [records, procs]) then
                        error(ertyp)
                     else
                        begin  (* search for field or entry identifier *)
								j := searchblock(btab[v.ref].last,id);
                        if j=0 then
									error(erdec)
								else
									if v.typ = procs then
										if tab[j].typ <> entrys then
											error(ertyp);
                        v.typ := tab[j].typ;
                        v.ref := tab[j].ref;
                        a := tab[j].taddr;
                        if a <> 0 then
                           if tab[j].typ = entrys then
                              emit1typed(ldcon,a,adrs)
                           else
                              emit1(ixrec,a)
                        end;
                     insymbol
                     end
                  end
               else
                  begin  (* array selector *)
                  if sy <> lbrack then error(erlbrack);
                  repeat
                     insymbol;
                     expression(fsys+[comma,rbrack],x);
                     if v.typ <> arrays  then
                        error(ertyp)
                     else
                        begin
                        a := v.ref;
                        if atab[a].inxtyp <> x.typ  then
                           error(ertyp)
                        else
									begin
									if atab[a].inxref <> x.ref then
										error(ertyp);
                           emit1typed(ixary,a,x.typ)
									end;
                        v.typ := atab[a].eltyp; v.ref := atab[a].elref
                        end
                  until sy <> comma;
                  if sy = rbrack then
                     insymbol
                  else
                     error(errbrack);
                  end (* array selector *)
            until not (sy in [lbrack,period]);
            test(fsys,[],ersym);
         end;  (* selector *)



            procedure actparams(var cp,lastp: integer);

            var
               k: integer;
               x: item;

               begin  (* Actparams *)
               repeat
                  insymbol;
                  if cp >= lastp  then
                     error(erpar)
                  else
                     begin
                     cp := cp+1;
                     if tab[cp].normal then
                        begin
                        (* value parameter *)
                        expression(fsys+[comma,colon,rparent],x);
                        if x.typ=tab[cp].typ  then
                           begin
									if x.typ = enums then
										begin
										if x.ref <> tab[cp].auxref then
											error(ertyp)
										end
									else
                           	if x.ref<>tab[cp].ref   then
                              	error(ertyp)
                           	else
                              	if x.typ = arrays then
                                 	emit1(ldblk,atab[x.ref].size)
                              	else
                                 	if x.typ=records then
                                    	emit1(ldblk,btab[x.ref].vsize)
												else
													if x.typ = synchros then
														emit1(ldblk,0);

                           end
                        else
									if (x.typ = ints) and (tab[cp].typ = reals) then
										emit1(ifloat,0)
									else
                           	if x.typ <> notyp then
                             		error(ertyp)
                        end  (* value parameter *)
                     else
                        begin
                        (*variable parameter*)
                        if sy <> ident  then
                           error(erident)
                        else
                           begin
                           k:= loc(id); insymbol;
                           if k <> 0 then
                              begin
                              if not (tab[k].obj in  [variable,address])   then
                                 error(erpar);
                              x.typ := tab[k].typ;
										if x.typ = enums then
											x.ref := tab[k].auxref
										else
											x.ref := tab[k].ref;
                              if (tab[k].obj = address) and
											not (tab[k].typ in [semafors,channels]) then
                                 emit1typed(ldcon,tab[k].taddr,adrs)
										else
                              	if tab[k].normal  then
                                 	emit2(ldadr,tab[k].lev,tab[k].taddr)
                              	else
                                 	emit2typed(ldval,tab[k].lev,tab[k].taddr,
                                                      adrs);
                              if sy in [lbrack,period] then
                                 selector(fsys+[comma,colon,rparent],x);
										if x.typ = tab[cp].typ then
											begin
											if x.typ = enums then
												begin
												if x.ref <> tab[cp].auxref then
													error(ertyp)
												end
											else
												if x.ref <> tab[cp].ref then
													error(ertyp)
											end
										else
											error(ertyp);

                              end
                           end
                        end
                     end;
                  test([comma,rparent],fsys,ersym);
               until sy <> comma;
               if sy = rparent  then
                  insymbol
               else
                  error(errparent)
            end;  (* actparams *)



         procedure call(fsys: symset; i: integer);

            var
               p: item;
               lastp, cp:integer;
               isaprocess: boolean;
               lc1: integer;



         begin  (* Call *)
            isaprocess := contains([procs],tab[i].typ,tab[i].ref);
            if isaprocess then
               begin
               lc1 := lc;
               emit2(mrkstk,1,0);  (* markstack for process *)
               emit2(ldadr,tab[i].lev, tab[i].taddr);
               p.typ := tab[i].typ; p.ref := tab[i].ref;
               if sy = lbrack then
                  selector([lparent,endsy]+fsys+statbegsys,p);
					if p.typ <> procs then error(ernotprocvar);
               cp := btab[p.ref].tabptr;
               code[lc1].y := cp;
               emit0(procv);
               lastp := btab[p.ref].lastpar;
               end
            else
               begin  (* not a process *)
               emit2(mrkstk,0,i);  (* markstack for procedure/function *)
               lastp := btab[tab[i].ref].lastpar;
					if tab[i+1].typ = protq then
						cp := i + 1
					else
               	cp := i
               end;
            if sy = lparent  then
               actparams(cp,lastp);
            if cp < lastp  then
                error(erpar); (* too few actual parameters *)
            if isaprocess then
               emit2(callsub,1,btab[p.ref].psize-intsize)
            else
               emit2(callsub,0,btab[tab[i].ref].psize-intsize);
            if tab[i].lev < codelevel  then
               emit2(updis,tab[i].lev,codelevel)
         end;  (* call *)

         procedure capscall(i: index);

         (* call exported capsule procedure *)
			(* i points to tab entry of capsule *)

         var
            j: integer;

         begin
            if sy = period then insymbol else error(erperiod);
            if sy = ident then
               begin
            	j := searchblock(btab[tab[i].ref].last,id);
               if (j = 0) or not (tab[j].obj  in [monproc,xgrdproc]) then
                  error(erdec)
               else
                  begin
						if tab[j].obj = xgrdproc then
							if (curcaps <> 0) and (tab[curcaps].typ = protvars) then
								error(ergrdcall);
						if i <> curcaps then
							begin
                  	emit2(ldadr,tab[i].lev,tab[i].taddr);
                  	emit0(enmon)
							end;
                  insymbol;
                  call(fsys, j);
						if i <> curcaps then
							if tab[i].typ = monvars then
                  		emit0(exmon)
							else
								begin
								emit1(prtcnd,tab[i].auxref);
								emit0(prtex)
								end
                  end
               end
         end;  (* capscalL *)




         procedure entrycall(fsys: symset; i: integer);

			(* parse entry call.  Only entered when tab[i].typ
			   contains a process *)

         var
            e: item;
            cp, lastp: integer;

         begin
            emit2(ldadr,tab[i].lev,tab[i].taddr);
            e.typ := tab[i].typ; e.ref := tab[i].ref;
            if sy in [period,lbrack] then
					begin
               selector(fsys+[lparent],e);
					if level = 1 then error(erlev)
					end
				else
					error(erent);
				if e.typ = entrys then
					begin
            	lastp := btab[e.ref].lastpar;
            	cp := btab[e.ref].tabptr;
            	if sy = lparent then actparams(cp,lastp);
            	if cp < lastp  then
                	error(erpar); (* too few actual parameters *)
            	emit1(ecall,btab[e.ref].psize)
					end
				else
					skip([semicolon]+fsys,erent)
         end;  (* entrycall *)



         function resulttype(a,b:types):types;

			(* entered with op in [plus,minus,times] *)

         begin
            if (not (a in [notyp,ints,reals,bitsets])) or
								(not (b in [notyp,ints,reals,bitsets])) then
               begin
               error(ertyp); resulttype := notyp
               end
            else
               if (a=notyp) or (b=notyp)  then
                  resulttype := notyp
               else
						case a of
							ints:
								if b = ints then
									resulttype := ints
								else
									if b = reals then
										begin
										resulttype := reals;
										emit1(ifloat,1)
										end
									else
										begin
										error(ertyp);
										resulttype := notyp
										end;
							reals:
								begin
								resulttype := reals;
								if b = ints then
									emit1(ifloat,0)
								else
									if b <> reals then
										begin
										error(ertyp);
										resulttype := notyp
										end
								end;
							bitsets:
								if b = bitsets then
									resulttype := bitsets
								else
									begin
									error(ertyp);
									resulttype := notyp
									end
						end  (* case *)
         end;  (* resulttype *)

         procedure expression;

         var
            y:item; op:symbol;

            procedure simpleexpression(fsys:symset; var x:item);

            var
               y:item; op:symbol;

                 procedure term(fsys:symset; var x:item);

               var
                  y:item; op:symbol;


                  procedure factor(fsys:symset; var x:item);

                  var
                     i: integer;

                     procedure standfun(i: integer);

                     (* standard functions *)
							(* i points to tab entry for the function *)

                     var
								n: integer;
								v: item;
								ts: typset;

                     begin  (* Standfun *)
								n := tab[i].taddr;
								if n in [17,18,25] then  (* no parameters *)
									emit1typed(stfun,n,tab[i].typ)
								else
									begin  (* parameter processing *)
									if sy = lparent then insymbol else error(erlparent);
                           expression(fsys+[rparent],v);
                        	case n of
									0,2:	(* abs, sqr *)
										begin
										ts := [ints,reals];
										if v.typ in ts then
											tab[i].typ := v.typ;
										if v.typ = reals then
											n := n + 1
										end;
									4,5:  (* odd, char *)
										ts := [ints];
									6:  (* ord *)
										ts := [ints,chars,bools,enums];
									7:	(* succ *)
										begin
										ts := [ints,bools,chars,enums];
										if v.typ in ts then
											begin
											tab[i].typ := v.typ;
											case v.typ of
												ints:
													emit1typed(hibnd,intmax-1,ints);
												bools:
													emit1typed(hibnd,fals,bools);
												chars:
													emit1typed(hibnd,charh-1,chars);
												enums:
													emit1typed(hibnd,bounds[v.ref].upper-1,ints)
											end  (* case *)
											end (* if in ts *)
										end;
									8:	(* pred *)
										begin
										ts := [ints,bools,chars,enums];
										if v.typ in ts then
											begin
											tab[i].typ := v.typ;
											case v.typ of
												ints:
													emit1typed(lobnd,-intmax+1,ints);
												bools:
													emit1typed(lobnd,tru,bools);
												chars:
													emit1typed(lobnd,charl+1,chars);
												enums:
													emit1typed(lobnd,1,ints)
											end  (* case *)
											end  (* if in ts *)
										end;
									9,10,11,12,13,14,15,16:
										begin
										ts := [ints,reals];
										if v.typ = ints then
											emit1(ifloat,0)
										end;
                        	19:  (* random *)
										ts := [ints];
                        	20:  (* empty *)
										ts := [condvars];
                        	21:  (* bits *)
										ts := [ints];
									24:	(* int *)
											ts := [bitsets]
                        	end;  (* case *)
									if v.typ in ts then
										emit1typed(stfun,n,tab[i].typ)
									else
										if v.typ <> notyp then error(ertyp);
									if sy = rparent then insymbol else error(errparent)
									end;  (* parameter processing *)
								x.typ := tab[i].typ;
								if x.typ = enums then
									x.ref := v.ref
                     end;  (* standfun *)


						procedure setlit(fsys: symset; var v: item);

						var
							e: item;
							basetyp: types;

						begin  (* Setlit *)
							insymbol;
							if sy = rbrack then
								begin  (* empty set *)
								emit1typed(ldcon,0,ints);
								insymbol
								end  (* empty set *)
							else
								begin  (* not empty set *)
								expression(fsys,e);
								if e.typ = ints then
									basetyp := ints
								else
									basetyp := notyp;
								emit0(power2);
								while sy = comma do
									begin
									insymbol;
									expression(fsys,e);
									if e.typ <> ints then
										basetyp := notyp;
									emit0(power2);
									emit0typed(orop,bitsets)
									end;  (* while sy = comma *)
								if basetyp = notyp then error(ersetlit);
								if sy = rbrack then
									insymbol
								else
									error(errbrack)
								end;  (* not empty set *)
							v.typ := bitsets;
							v.ref := 0
						end;  (* setlit *)



                  begin  (* Factor *)
                     x.typ := notyp; x.ref := 0;
                     test(facbegsys,fsys,ersym);
                     while sy in facbegsys do
                        begin
                        case sy of

								ident:
                           begin
                           i := loc(id); insymbol;
                           with tab[i] do
                              case obj of
                                 konstant:
                                       begin
                                       x.typ := typ;
													if x.typ = enums then
														x.ref := auxref
													else
														x.ref := 0;
                                       emit1typed(ldcon,taddr,x.typ)
                                       end;

                                 variable:
                                       begin
                                       x.typ := typ;
													if x.typ = enums then
														x.ref := auxref
													else
														x.ref := ref;
                                       if sy in [lbrack, period] then
                                          begin  (* structured type *)
                                          if normal  then
                                             emit2(ldadr,lev,taddr)
                                          else
                                             emit2typed(ldval,lev,taddr,adrs);
                                          selector(fsys,x);
                                          if x.typ in simpletyps then
                                             emit0typed(repadr,x.typ)
                                          end  (* structured type *)
                                       else
                                          begin
                                          if x.typ in simpletyps then
                                             if normal   then
                                                emit2typed(ldval,lev,taddr,x.typ)
                                             else
                                                emit2typed(ldind,lev,taddr,x.typ)
                                          else
                                             if normal then
                                                emit2(ldadr,lev,taddr)
                                             else
                                                emit2typed(ldval,lev,taddr,adrs)
                                          end
                                       end;

                              	address:
                                       begin
                                       x.typ := typ;
													if x.typ = enums then
														x.ref := auxref
													else
														x.ref := ref;
													if typ = semafors then
														emit2(ldadr,lev,taddr)
													else
                                       	emit1typed(ldcon,taddr,adrs);
                                       if sy in [lbrack, period] then
                                          selector(fsys,x);
                                       if x.typ in simpletyps then
                                          emit0typed(repadr,x.typ)
                                       end;

                              	type1,
											prozedure,
											monproc,
											xgrdproc,
											grdproc:
                                       error(ertyp);

                              	funktion:
                                       if lev <> 0  then
														begin
														x.typ := typ;
														if x.typ = enums then
															x.ref := auxref;
                                          call(fsys,i)
														end
                                       else
                                          standfun(i)
                              end  (* case obj *)
                           end; (* sy wa ident *)

								realcon,
                        charcon,
								intcon:
									begin
									if sy = realcon then
										begin
										x.typ := reals;
										enterreal(rnum);
										emit1typed(ldcon,realindex,reals)
										end
									else
										begin
                              if sy = charcon then
                                 x.typ:= chars
                              else
                                 x.typ:= ints;
                              emit1typed(ldcon,inum,x.typ)
										end;  (* charcon, intcon *)
                           x.ref:= 0;insymbol
                           end ;  (* intcon, realcon, charcon *)

								lbrack:
										setlit(fsys+[comma,rbrack],x);

                        lparent:
                              begin
                              insymbol; expression(fsys+[rparent],x);
                              if sy = rparent then
                                 insymbol
                              else
                                 error(errparent)
                              end ;  (* lparent *)

                        notsy:
                              begin
                              insymbol; factor(fsys,x);
                              if x.typ=bools  then
                                 emit0typed(notop,bools)
                              else
                                 if x.typ <> notyp   then
                                    error(ertyp)
                              end  (* notsy *)
								end;  (* case sy  *)
                        test(fsys,facbegsys,ersym);
                        end (* while sy in facbegsys *)
                  end;  (* factor *)

               begin  (* Term *)
                  factor(fsys+[times,idiv,rdiv,imod,andsy],x);
                  while sy in [times,idiv,rdiv,imod,andsy] do
                     begin
                     op := sy; insymbol;
                     factor(fsys+[times,idiv,rdiv,imod,andsy],y);
                     case op of
                     times:
                        begin
                        x.typ := resulttype(x.typ,y.typ);
                        if x.typ in [ints,reals,bitsets]  then
									if x.typ = bitsets then
										emit0typed(andop,bitsets)
									else
                           	emit0typed(mul,x.typ)
                        end;
                     andsy:
                        begin
                        if (x.typ = bools) and (y.typ = bools) then
                           emit0typed(andop,bools)
                        else
                           begin
                           if (x.typ <> notyp) and (y.typ <> notyp) then
                              error(ertyp);
                           x.typ := notyp
                           end
                        end;
                     idiv,imod:
                        begin
                        if (x.typ = ints) and (y.typ = ints) then
                           if op=idiv  then
                              emit0typed(divop,ints)
                           else
                              emit0typed(modop,ints)
                        else
                           begin
                           if (x.typ <> notyp) and (y.typ <> notyp) then
                              error(ertyp);
                           x.typ:= notyp
                           end
                        end;
							rdiv:
								begin
								if y.typ = ints then
									begin
									emit1(ifloat,0);
									y.typ := reals
									end;
								if x.typ = ints then
									begin
									emit1(ifloat,1);
									x.typ := reals
									end;
								if (x.typ = reals) and (y.typ = reals) then
									emit0typed(divop,reals)
								else
									begin
									if (x.typ <> notyp) and (y.typ <> notyp) then
										error(ertyp);
									x.typ := notyp
									end
								end
                     end  (* case *)
                     end  (* while *)
               end; (* term *)

            begin   (* Simpleexpression *)
               if sy in [plus,minus] then
                  begin
                  op := sy; insymbol;
                  term(fsys+[plus, minus], x);
                  if not (x.typ in [notyp,ints,reals]) then
                     error(ertyp)
                  else
                     if op = minus then
                        emit0typed(negate,x.typ)
                  end
               else
                  term(fsys+[plus, minus, orsy], x);
               while sy in [plus, minus, orsy] do
                  begin
                  op := sy; insymbol;
                  term(fsys + [plus, minus,orsy], y);
                  if op = orsy  then
                     begin
                     if (x.typ = bools) and (y.typ = bools)  then
                        emit0typed(orop,bools)
                     else
                        begin
                        if (x.typ <> notyp) and (y.typ <> notyp)  then
                           error(ertyp);
                        x.typ := notyp
                        end
                     end   (* if op = orsy *)
                  else
                     begin   (* sy in [plus,minus] *)
                     x.typ := resulttype(x.typ, y.typ);
                     if x.typ in [ints,reals,bitsets] then
                        if op = plus then
									if x.typ = bitsets then
										emit0typed(orop,bitsets)
									else
                           	emit0typed(add,x.typ)
                        else
                           emit0typed(sub,x.typ)
                     end
                  end   (* while sy in plus, minus, orsy *)
            end;  (* simpleexpression *)

 begin  (* Expression *)
            simpleexpression(fsys + [eql,neq,lss,leq,gtr,geq,insy], x);
            if sy in [eql,neq,lss,leq,gtr,geq,insy]  then
               begin
               op := sy; insymbol; simpleexpression(fsys, y);
					if op = insy then
						if (x.typ <> ints) or (y.typ <> bitsets) then
							begin
							if (x.typ <> notyp) and (y.typ <> notyp) then
								error(ertyp)
							end
						else
							emit0(btest)
					else
               	if x.typ in simpletyps then
							begin
							if (x.typ = enums) and (y.typ = enums)  then
								begin
								if x.ref <> y.ref then error(ertyp)
								end
							else
								if (x.typ = ints) and (y.typ = reals) then
									begin
									x.typ := reals;
									emit1(ifloat,1)
									end
								else
									if (x.typ = reals) and (y.typ = ints) then
										begin
										y.typ := reals;
										emit1(ifloat,0)
										end;
							if x.typ <> y.typ then
								begin
								if (x.typ <> notyp) and (y.typ <> notyp) then
									error(ertyp)
								end
							else
                  		case op of
                     		eql :
                           		emit0typed(relequ,x.typ);
                     		neq :
                           		emit0typed(relneq,x.typ);
                     		lss :
                           		emit0typed(rellt,x.typ);
                     		leq :
                           		emit0typed(relle,x.typ);
                     		gtr :
                           		emit0typed(relgt,x.typ);
                     		geq :
                           		emit0typed(relge,x.typ);
								end  (* case *)
                  	end
               	else
                  	error(ertyp);
               x.typ := bools
               end
         end;  (* expressioN *)

 (*---------------------------------------------------------------statement-*)

      procedure statement(fsys: symset);

      var
         i: integer;









         procedure channelop2(tptr: integer; x: item; inselect: boolean);

         (* second part of channel operation parser *)
         (* entered from assignment or channelop with x a channel *)

         var
            basetype: types;
            baseref, basesize: index;
            k,extra: integer;
            y: item;

         begin (* Channelop2 *)
				if tab[tptr].obj = address then
					extra := 4
				else
					extra := 0;
            with chantab[x.ref] do
               begin
               basetype := eltyp;
               baseref := elref;
               basesize := elsize
               end;
            if not (sy in [shriek, query]) then
               skip([semicolon],ersym)
            else
               if sy= query then
                  begin
                  insymbol;
                  if sy=ident then
                     begin
                     k := loc(id);
                     if k = 0 then
                        skip([semicolon],erdec)
                     else
                        with tab[k] do
                           if not(obj in [variable,address]) then
                              skip([semicolon],ervar)
                           else
                              begin  (* obj in [variable,address] *)
                              y.typ:=typ;
										if y.typ = enums then
											y.ref := auxref
										else
											y.ref:=ref;
										if obj = variable then
                              	if normal then
                                 	emit2(ldadr,lev,taddr)
                              	else
                                 	emit2typed(ldval,lev,taddr,adrs)
										else
											emit1typed(ldcon,taddr,adrs);
                              insymbol;
                              if sy in [lbrack,period] then
                                 selector(fsys+[semicolon,orsy,endsy],y);
                              if (y.typ=basetype) and (y.ref=baseref) then
                                 if inselect then
                                    begin


												emit2(selec1,3,2+extra);
												emit2(selec1,4,basesize)
                                    end
                                 else
												emit2(chanrd,extra,basesize)
                              else
                                 error(ertyp)
                              end  (* obj in [variable,address] *)
                     end  (* if sy = ident *)
                  else
                     skip([semicolon,orsy,endsy],erident);
                  end  (* sy was query *)
               else
                  begin  (* sy is shriek *)
                  insymbol;
                  expression(fsys+[elsesy],y);
                  if (y.typ=basetype) and (y.ref=baseref) then
                     if y.typ in simpletyps then
                        if inselect then
                           begin


									emit2(selec1,3,extra);
									emit2(selec1,4,basesize)
                           end
                        else
                           emit2typed(chanwr,extra,basesize,y.typ)
                     else
                        if inselect then
                           begin

									emit2(selec1,3,1+extra);
									emit2(selec1,4,basesize)
                           end
                        else
                           emit2typed(chanwr,extra,basesize,y.typ)
                  else
                     error(ertyp)
                  end (* sy was shriek *)
         end;  (* channelop2 *)

          procedure channelop;

          (* first part of channel operation parser *)
          (* entered from selstatement with sy=id *)

          var
            i: integer;
            x: item;

         begin  (* Channelop *)
            i := loc(id);
            if i = 0 then
               skip([semicolon],erdec)
            else
               with tab[i] do
                  if not (obj in [variable, address]) then
                     skip([semicolon],ervar)
                  else
                     begin  (* obj in [variable,address] *)
                     insymbol;
                     x.typ := typ; x.ref := ref;
                     if normal then
                        emit2(ldadr,lev,taddr)
                     else
                        emit2typed(ldval,lev,taddr,adrs);
                     if sy in [lbrack, period] then
                        selector([becomes,eql,shriek,query]+fsys,x);

                     if x.typ=channels then
                        channelop2(i,x,true)
                     else
                        skip([semicolon],ertyp)
                     end (* obj in [variable,address] *)
         end;   (* channelop *)


         procedure assignment(lv, ad : integer);

         var
            x, y : item;


         begin  (* Assignment *)
            x.typ := tab[i].typ;
				if x.typ = enums then
					x.ref := tab[i].auxref
				else
					x.ref := tab[i].ref;
            if (tab[i].obj=address) and (x.typ <> channels)  then
               emit1typed(ldcon,tab[i].taddr,adrs)
            else
               if tab[i].normal then
                  emit2(ldadr,lv,ad)
               else
                  emit2typed(ldval,lv,ad,adrs);
            if sy in [lbrack,period] then
               selector([shriek,query,becomes, eql] + fsys, x);
            if x.typ=channels then
               channelop2(i,x,false)
            else
               begin
               if contains([semafors,channels,condvars],x.typ,x.ref) then
                  error(erassign);
               if sy = becomes  then
                  insymbol
               else
                  error(erbecomes);
               expression(fsys, y);
               if x.typ = y.typ then
                  begin
                  if x.typ in simpletyps  then
							begin
                     emit0typed(store,x.typ);
							if (x.typ = enums) and (x.ref <> y.ref) then
								error(ertyp)
							end
                  else
                     if x.ref <> y.ref then
                        error(ertyp)
                     else
                        if x.typ = arrays then
                           emit1(cpblk, atab[x.ref].size)
                        else
                           if x.typ=records then
                              emit1(cpblk,btab[x.ref].vsize)
									else
										if x.typ = synchros then
											emit1(cpblk,0)
                  end  (* x.typ = y.typ *)
               else
						if (x.typ = reals) and (y.typ = ints) then
							begin
							emit1(ifloat,0);
							emit0typed(store,reals)
							end
						else
							if y.typ <> notyp then
                  		error(ertyp)
               end
         end;  (* assignment *)


         procedure compoundstatement;

         begin
            insymbol;
            statement([semicolon, endsy] + fsys);
            while sy in [semicolon] + statbegsys do
               begin
               if sy = semicolon  then
                  insymbol
               else
                  error(ersemi);
               statement([semicolon, endsy] + fsys)
               end;
            if sy = endsy  then
               insymbol
            else
               error(erend)
         end;  (* compoundstatement *)


         procedure ifstatement;

         var
            x:item;lc1,lc2:integer;

         begin
            insymbol;
            expression(fsys+[thensy,dosy],x);
            if not (x.typ in [bools, notyp]) then
               error(ertyp);
            lc1 := lc;emit0(jmpiz);(*jmpc*)
            if sy = thensy then
               insymbol
            else
               error(erthen);
            statement(fsys+[elsesy]);
            if sy = elsesy then
               begin
               insymbol;lc2 := lc; emit0(jmp);
               code[lc1].y:=lc;

					statement(fsys);code[lc2].y := lc
               end
            else
               code[lc1].y := lc;

         end;  (* ifstatement *)

			procedure casestatement;

			var
				x: item;
				i, j, k, lc1: integer;
				casetab: array[1..casemax] of
								packed record
									val, lc: index
								end;
				exittab: array[1..casemax] of integer;


				procedure caselabel;

				var
					lab: conrec;
					k: integer;

				begin
					constant(fsys+[comma,colon],lab);
					if (lab.tp <> x.typ) or
							((lab.tp = enums) and (lab.ref <> x.ref))  then
						error(ertyp)
					else
						if i = casemax then
							fatal(13)
						else
							begin
							i := i + 1;
							k := 0;
							casetab[i].val := lab.i;
							casetab[i].lc := lc;
							repeat
								k := k + 1
							until casetab[k].val = lab.i;
							if k < i then error(ercasedup);
							end
				end;  (* caselabel *)


				procedure onecase;

				begin
					if sy in constbegsys then
						begin
						caselabel;
						while sy = comma do
							begin
							insymbol;
							caselabel
							end;
						if sy = colon then insymbol else error(ercolon);

						statement([semicolon,endsy]+fsys);
						j := j + 1;
						exittab[j] := lc;
						emit0(jmp)
						end
				end;  (* onecase *)

				begin  (* Casestatement *)
					insymbol;
					i := 0; j := 0;
					expression(fsys+[ofsy,comma,colon],x);
					if not (x.typ in [ints,bools,chars,enums,notyp]) then
						begin
						error(ertyp);
						x.typ := notyp
						end;
					lc1 := lc;
					emit0(jmp);
					if sy = ofsy then insymbol else error(erof);
					onecase;
					while sy = semicolon do
						begin
						insymbol;
						onecase
						end;
					code[lc1].y := lc;

					for k := 1 to i do
						begin
						emit1typed(ldcon,casetab[k].val,x.typ);
						emit1typed(case1,casetab[k].lc,x.typ)
						end;
					emit0(case2);
					for k := 1 to j do
						code[exittab[k]].y := lc;

					if sy = endsy then insymbol else error(erend)
				end;  (* casestatement *)

         procedure repeatstatement;

         var
            x: item;lc1:integer;
				nestedloops: boolean;

         begin
				nestedloops := inaloop;
				inaloop := true;
            lc1 := lc;

            insymbol; statement([semicolon, untilsy, foreversy]+fsys);
            while sy in [semicolon]+statbegsys do
               begin
               if sy = semicolon  then
                    insymbol
               else
                  error(ersemi);
               statement([semicolon, untilsy, foreversy]+fsys)
               end;
            if sy = untilsy  then
               begin
               insymbol;expression(fsys, x);
               if not (x.typ in [bools, notyp]) then
                  error(ertyp);
               emit1(jmpiz,lc1)
               end
            else
               if sy=foreversy then
                  begin
                  emit1(jmp, lc1);
                  insymbol
                  end
               else
                  error(eruntil);
				inaloop := nestedloops
         end; (* repeatstatement *)

         procedure whilestatement;

         var
            x : item;lc1,lc2:integer;
				nestedloops: boolean;

         begin
				nestedloops := inaloop;
				inaloop := true;
            insymbol;lc1 := lc;
            expression(fsys+[dosy],x);
            if not (x.typ in [bools, notyp]) then
               error(ertyp);
            lc2 := lc;emit0(jmpiz);
            if sy = dosy then
               insymbol
            else
               error(erdo);
            statement(fsys); emit1(jmp, lc1); code[lc2].y := lc;
				inaloop := nestedloops
         end;  (* whilestatement *)

         procedure forstatement;

         var
            cvt: types;
				x: item;
            i, lc1,lc2, rf: integer;
				nestedloops: boolean;

         begin
				cvt := notyp;  (* default in case of errors *)
				nestedloops := inaloop;
				inaloop := true;
            insymbol;
            if sy = ident  then
               begin
               i:= loc(id);
               if i <> 0 then
                  if tab[i].obj = variable then
                     begin
                     cvt := tab[i].typ;
							if cvt = enums then
								rf := tab[i].auxref
							else
								rf := tab[i].ref;
                     if not tab[i].normal then
                        error(erinx)
                     else
                        emit2(ldadr, tab[i].lev, tab[i].taddr);
                     if not (cvt in [notyp, ints, bools, chars,enums])  then
								begin
								cvt := notyp;
                        error(ertyp)
								end
                     end (* obj was variable *)
                  else
							begin  (* not variable *)
                     error(ervar);
							cvt := notyp
							end;
					insymbol
               end  (* sy was ident *)
            else
               skip([becomes, tosy, dosy]+fsys, erident);
            if sy = becomes  then
               begin
               insymbol; expression([tosy, dosy]+fsys, x);
               if (x.typ <> cvt) and (cvt <> notyp) then
                  error(ertyp)
					else
						if cvt = enums then
							if x.ref <> rf then error(ertyp)
               end
            else
               skip([tosy, dosy]+fsys, erbecomes);
            if sy = tosy then
               begin
               insymbol; expression([dosy]+statbegsys+fsys,x);
               if (x.typ <> cvt) and (cvt <> notyp)  then
                  error(ertyp)
					else
						if cvt = enums then
							if x.ref <> rf then
								error(ertyp)
               end
            else
               skip([dosy]+fsys,erto);
            lc1 := lc; emit1typed(for1up,lc1,cvt);
            if sy = dosy then
               insymbol
            else
               error(erdo);
            lc2 := lc;

				statement(fsys);
            emit1typed(for2up,lc2,cvt);
				code[lc1].y := lc;

				inaloop := nestedloops
         end;  (* forstatement *)


  procedure acceptstatement;

      (* Ada-like accept statement *)

      var
         i, extra: integer;
			err: boolean;

      begin
			if not inprocessdec or (codelevel <> 2) then
				error(eracptinproc);
			extra := 0;
			err := false;
         insymbol;
         if sy <> ident then
            skip(fsys,erident)
         else
            begin
            i := find(id);
            insymbol;
				if i = 0 then
					begin
					err := true;
					skip([dosy]+fsys,erdec)
					end
				else
            	if tab[i].typ <> entrys then
						begin
						err := true;
               	skip([dosy]+fsys,ertyp)
						end
					else
						begin  (* is an entry *)
						if tab[i].auxref <> 0 then
							error(ernestacpt);
						tab[i].auxref := 1;
						if tab[i].obj = address then
							extra := 4;
            		parametercheck(i)
						end;  (* is an entry *)
            if sy = dosy then insymbol else error(erdo);
				if err then
					statement([semicolon,endsy]+fsys)
				else
					begin  (* no error *)
            	level := level + 1;
            	display[level] := tab[i].ref;
					emit2(ldadr,tab[i].lev,tab[i].taddr);
            	emit2(acpt1,extra,btab[tab[i].ref].psize);
            	statement([semicolon,endsy]+fsys);
            	level := level - 1;
            	emit2(ldadr,tab[i].lev,tab[i].taddr);
            	emit2(acpt2,extra,btab[tab[i].ref].psize);
					tab[i].auxref := 0
					end (* no error *)
            end  (* sy was ident *)
      end;  (* acceptstatement *)


      procedure acceptinselect(var k: index);

      (* Ada-like accept statement in select statement *)

      var
			i, extra: integer;
			h,j: index;

			err: boolean;


      begin
			if not inprocessdec or (codelevel <> 2) then
				error(eracptinproc);
			extra := 0;
			err := false;

			j := 0;
         insymbol;
         if sy <> ident then
				begin
            skip(fsys,erident);
				k := lc  (* do not return with k undefined *)
				end
         else
            begin
            i := find(id);
            insymbol;
				if i = 0 then
					begin
					err := true;
					skip([dosy]+fsys,erdec)
					end
				else

            	if tab[i].typ <> entrys then

						begin
						err := true;
               	skip([dosy]+fsys,ertyp)
						end
					else
						begin  (* is entry *)

						if tab[i].auxref <> 0 then
							error(ernestacpt);
						tab[i].auxref :=  1;
						if tab[i].obj = address then
							extra := 4;
            		parametercheck(i)
						end;  (* is entry *)

            if sy = dosy then insymbol else error(erdo);
				if err then
					begin
					k := lc; (* do not return with k undefined *)
					statement([semicolon,elsesy,endsy]+fsys);
					end
				else
					begin  (* no error *)
            	level := level + 1;
            	display[level] := tab[i].ref;
           		emit2(ldadr,tab[i].lev,tab[i].taddr);

					emit1typed(ldcon,0,ints);  (* data not used in ada *)


					emit2(selec1,3,3+extra);
					emit2(selec1,4,btab[tab[i].ref].psize);
					h := lc;
					emit2(selec1,5,0);
					emit1typed(ldcon,0,ints);  (* rep index not used in ada *)

					j := lc;
            	emit1(jmp,0);   (* address supplied by oneselect *)
					code[h].y := lc;


					emit2(ldadr,tab[i].lev,tab[i].taddr);
            	emit1(acpt1,btab[tab[i].ref].psize);
            	statement([semicolon,elsesy,endsy]+fsys);

            	level := level - 1;

            	emit2(ldadr,tab[i].lev,tab[i].taddr);
            	emit1(acpt2,btab[tab[i].ref].psize);

					tab[i].auxref := 0;
					k := j
					end (* no error *)
            end  (* sy was ident *)
      end;  (* acceptinselect *)





      procedure selstatement;

      (* parser for select statement *)

      var
         ends: array[1..casemax] of index;
         c: 0..casemax;
         f, loop: integer;

         priority, term, time: boolean;


         procedure oneselect;

         (* parse one select alternative *)

         var
            x: item;
            guard, rep: boolean;
            i: integer;
            g,h,k: index;
            replc, repcj:  0..cmax;
				cvt: types;

            procedure repstart(var i: integer; var cvt: types);

            (* leading code for replicate alternative *)

            var
					rf: integer;
               x: item;

            begin
					cvt := notyp;  (* default in case of errors *)
               insymbol;
               if sy = ident then
                  begin
                  i := loc(id);
                  if i = 0 then
                     cvt := notyp
                  else
                     if tab[i].obj = variable then
                        begin
                        cvt := tab[i].typ;
								if cvt = enums then
									rf := tab[i].auxref
								else
									rf := tab[i].ref;
                        if not tab[i].normal then
                           error(erinx)
                        else
                           emit2(ldadr,tab[i].lev,tab[i].taddr);
                        if not (cvt in [notyp, ints,chars,bools,enums]) then
									begin
                           error(ertyp);
									cvt := notyp
									end
                        end  (* obj was variable *)
                     else
                        begin  (* not variable *)
                        error(ervar);
                        cvt := notyp
                        end;
                  insymbol;
                  end  (* if sy = ident *)
               else
                  skip([becomes,tosy,dosy]+fsys,erident);
               if sy = becomes then
                  begin
                  insymbol;
                  expression([tosy,dosy]+fsys,x);
                  if (x.typ <> cvt) and (cvt <> notyp)  then
							error(ertyp)
						else
							if x.typ = enums then
								if x.ref <> rf then
									error(ertyp)
                  end  (* sy = becomes *)
               else
                  skip([tosy,dosy]+fsys,erbecomes);
               emit0typed(store,cvt);
               replc := lc;

               emit2typed(ldval,tab[i].lev,tab[i].taddr,cvt);
               if sy = tosy then insymbol else error(erto);
               expression([whensy,replicatesy,dosy]+fsys,x);
               if (x.typ <> cvt) and (cvt <> notyp)  then
						error(ertyp)
					else
						if cvt = enums then
							if x.ref <> rf then
								error(ertyp);
               emit0typed(relle,cvt);
               repcj := lc;
               emit0(jmpiz);  (* address comes later *)
               if sy = replicatesy then insymbol else error(erreplicate)
            end;  (* repstart *)

            procedure repend(i: integer; cvt: types);

            (* trailing code for replicate alternative *)

            begin
               emit2(ldadr,tab[i].lev,tab[i].taddr);
               emit1typed(rep2c,replc,cvt);
               code[repcj].y := lc;

            end;



         begin  (* Oneselect *)
            if sy = forsy then
               begin
               rep := true;
               repstart(i,cvt);
               end
            else
               rep := false;
            guard := sy=whensy;
            if guard then
               begin
               insymbol;
               expression(fsys+[arrow,becomes],x);
               if x.typ <> bools then error(ertyp);
               if sy=arrow then insymbol else error(ersym);
               g:=lc;
               emit1(jmpiz,0);   (* address of next select comes later *)
               end;  (* guard found *)
				if sy = forsy then
					begin
					error(ersym);
					repstart(i,cvt)
					end;
            if sy in [ident,timeoutsy,acceptsy] then
               begin
               if sy=ident then
                  begin  (* channel alternative *)
                  channelop;
						k := lc;
						emit2(selec1,5,0);
                  if rep then

                     emit2typed(ldval, tab[i].lev,tab[i].taddr,cvt)


                  else

							emit1typed(ldcon,0,ints);

                  h := lc;
                  emit1(jmp,0);
						code[k].y := lc
                  end  (* channel alternative *)
               else
                  if sy = acceptsy then
							begin
							if rep then error(ersym);
                     acceptinselect(h)
							end
                   else
                     begin  (* timeout alternative *)
							if rep then error(ersym);
                     if term then error(ertimetermelse);
                     time := true;
                     insymbol;
                     emit1typed(ldcon,0,ints);  (* chanptr *)

                     emit1typed(ldcon,0,ints);  (* dataptr *)


                     emit1typed(ldcon,0,ints);  (* trantype *)


                     expression(fsys+[semicolon,orsy,elsesy,endsy],x);


                     if x.typ <> ints then error(ertyp);
							k := lc;
							emit2(selec1,5,0);
							emit1typed(ldcon,0,ints);

							h := lc;
							emit1(jmp,0);  (* address comes later *)
							code[k].y := lc
                     end;

					if rep then
						emit2typed(rep1c,tab[i].lev,tab[i].taddr,cvt);
               while sy in (statbegsys+[semicolon,ident]) do
                  begin
                  if sy=semicolon then insymbol else error(ersemi);

                  statement(fsys+[semicolon,orsy,endsy,elsesy])

                  end;
               if c=casemax then fatal(8);
               c:=c+1;
               ends[c]:=lc;
               emit1(jmp,0);  (* gets select exit address later *)
               if guard then code[g].y:=lc;
               code[h].y:=lc;

               if rep then
                  repend(i,cvt)
               end  (* channel op alternative *)
            else
               if sy=termsy then
						begin
                  if guard or rep then
							error(ersym)
						else
                  	if time then
								error(ertimetermelse);
                  term:=true;
                  insymbol;
                  if sy=semicolon then
                     insymbol
                  else

                     if not (sy in [endsy,elsesy]) then

                        error(ersym);

                  test([orsy,endsy,elsesy],[],ersym)

						end
               else
                  skip([semicolon],ersym)
         end;  (* oneselect *)


      begin  (* Selstatement *)
         term:=false;
         time := false;
         priority:=sy=prisy;
         if priority then insymbol;
         c:=0;
         if sy=selectsy then insymbol else error(erselect);

			emit2(selec1,0,0);  (* sentinel *)
         oneselect;
         while sy=orsy do
            begin insymbol; oneselect end;
         if term then
            f:=1
         else
            if sy=elsesy then
               f:=2
            else
               f:=0;

         if priority then emit2(selec0,1,f) else emit2(selec0,0,f);
         if sy=elsesy then
            begin
            if term or time  then error(ertimetermelse);
            insymbol;
            statement(fsys+[semicolon,ident,endsy]);
            while sy in (statbegsys+[semicolon,ident]) do
               begin
               if sy=semicolon then insymbol else error(ersemi);
               statement(fsys+[semicolon,ident,endsy])
               end;
            if sy=semicolon then insymbol
            end;  (* else part *)

         if sy=endsy then insymbol else error(erend);
         for loop := 1 to c do code[ends[loop]].y:= lc;

      end; (* selstatement *)


 procedure requeuestatement;

		var
			i: integer;
				distref: integer;

		begin (* Requeuestatement *)
			distref := 0;
			if not inguardedproc or (level <> 3) then
				error(eronlyingrdproc);
			insymbol;
			if sy <> ident then
				begin
				error(erident)
				end
			else
				begin
				i := loc(id);
				if i = 0 then
					begin  (* identifier not found *)
					error(erdec)
					end
				else
					begin  (* could be dotted or simple notation *)
					if tab[i].obj = variable then
						begin  (* could be capsule name *)
						if tab[i].typ <> protvars then
							error(ertyp)
						else
							begin  (* resource name found - is it a local call? *)
							if i <> curcaps then
								distref := i;
							insymbol;
							if sy = period then insymbol else error(erperiod);
							if sy <> ident then
								error(erident)
							else
								begin  (* find procedure name *)
								i := searchblock(btab[tab[i].ref].last,id);
								if i = 0 then
									error(erdec)
								end
							end
						end;
					if tab[i].obj in [grdproc,xgrdproc] then
						begin
						insymbol;
						if distref <> 0 then
							begin  (* requeue to a different resource *)
							emit1(prtcnd,tab[curcaps].auxref);
							emit2(ldadr,tab[distref].lev,tab[distref].taddr);
							emit0(enmon)
							end;
						call(fsys,i);
						if distref <> 0 then
							begin
							emit1(prtcnd,tab[distref].auxref);
							emit2(prtex,1,0)
							end;
						emit0(retproc)
						end
					else
						error(ermustbeguarded)
					end
				end
		end;  (* requeuestatemenT *)


      procedure standproc(n:integer);

      var
         i, sptr: integer;
         x, v: item;
			based: boolean;

      begin  (* Standproc *)
         case n of
            1,2:
               begin
               (* read *)
               if sy = lparent then
                  begin
                  repeat
                     insymbol;
                     if sy <> ident then
                        error(erident)
                     else
                        begin
                        i:=loc(id);insymbol;
                        if i <> 0  then
                           if not(tab[i].obj in [variable,address]) then
                              error(ervar)
                           else
                              begin
                              x.typ:=tab[i].typ;x.ref:=tab[i].ref;
										if tab[i].obj = address then
											emit1typed(ldcon,tab[i].taddr,adrs)
										else
                              	if tab[i].normal  then
                                 	emit2(ldadr,tab[i].lev,tab[i].taddr)
                              	else
                                 	emit2typed(ldval,tab[i].lev,tab[i].taddr,adrs);
                              if sy in [lbrack, period] then
                                 selector(fsys+[comma,rparent],x);
                              if x.typ in [ints,reals,chars,notyp]  then
                                 emit0typed(readip,x.typ)
                              else
                                 error(ertyp)
                              end
                        end;
                     test([comma,rparent],fsys,ersym)
                  until sy<>comma;
                  if sy = rparent then
                     insymbol
                  else
                     error(errparent)
                  end;
               if n=2  then
                  emit0(rdlin);
               end;

            3,4:
               begin
               (* write *)
               if sy = lparent then
                  begin
                  repeat
                     insymbol;
                     if sy=string1 then
                        begin
								sptr := inum;
                        emit1typed(ldcon,sleng,ints);
                        insymbol ;
								if sy = colon then
									begin
									insymbol;
									expression(fsys+[comma,rparent],v);
									if v.typ <> ints then error(ertyp);
									emit1(wrsfm,sptr)
									end
								else
                        	emit1(wrstr,sptr);
                        end  (* string *)
                     else
                        begin
                        expression(fsys+[comma,colon,percent,rparent],x);
                        if not (x.typ in ((simpletyps+[semafors])-[enums])) then
									begin
                           error(ertyp);;
									x.typ := notyp
									end;
                        if x.typ = semafors then
                           emit0typed(repadr,semafors);
								if sy in [colon,percent] then
									begin
									if sy = percent then
										if not(x.typ in [ints,bitsets]) then
											begin
											error(ertyp);
											based := false
											end
										else
											based := true
									else
										based := false;
									insymbol;
									expression(fsys+[comma,colon,rparent],v);
									if v.typ <> ints then error(ertyp);
									if based then
										emit0typed(wrbas,x.typ)
									else
										begin  (* formatted output *)
										if sy = colon then
											begin
											if x.typ <> reals then
												error(ertyp);
											insymbol;
											expression(fsys+[comma,rparent],v);
											if v.typ <> ints then
												error(ertyp);
											emit0(w2frm)
											end
										else
											emit0typed(wrfrm,x.typ)
										end  (* formatted output *)
									end
								else
                        	emit0typed(wrval,x.typ)
                        end
                  until sy<>comma;
                  if sy=rparent   then
                     insymbol
                  else
                     error(errparent)
                  end;
               if n=4  then
                  emit0(wrlin)
               end;

            5,6,7,8,9:
               (* wait,signal,delay,resume,initial *)
					begin
						if n = 9 then		(* initial *)
							if inprocessdec then
								error(ernotinproc);
               if sy <> lparent then
                  error(erlparent)
               else
                  begin
                  insymbol;
                  if sy<>ident then
                     error(erident)
                  else
                     begin
                     i:=loc(id);insymbol;
                     if i<>0  then
                        if not(tab[i].obj in[variable,address]) then
                           error(ertyp)
                        else
                           begin
                           x.typ:=tab[i].typ; x.ref:=tab[i].ref;
                           if tab[i].normal  then
                              emit2(ldadr,tab[i].lev,tab[i].taddr)
                           else
                              emit2typed(ldval,tab[i].lev,tab[i].taddr,adrs);
                           if sy in [lbrack, period] then
                              selector(fsys+[comma,rparent],x);
                           if (x.typ=semafors) and (n in [5,6,9]) then
                              if n=9 then
                                 begin
                                 if sy=comma then insymbol else error(ersym);
                                 expression(fsys+[rparent],x);
											emit1typed(lobnd,0,ints);
                                 if not (x.typ in [ints, notyp]) then
                                    error(ertyp)
											else
                                 	emit0(sinit)
                                 end
                              else
                                 if n=5 then
												if tab[i].obj=address then
													emit2(wait,1,0)
												else
													emit0(wait)
											else
												emit0(signal)
                           else
                              if (x.typ=condvars) and (n in [7,8]) then
                                 if n=7 then emit0(delay) else emit0(resum)
                              else
                                 error(ertyp)
                           end
                     end;
                  if sy=rparent  then
                     insymbol
                  else
                     error(errparent)
                  end
					end;
            10,11:
                  begin    (* priority, sleep *)
                  if sy <> lparent then
                     error(erlparent)
                  else
                     begin
                     insymbol;
                     expression(fsys+[rparent],x);
                     if x.typ <> ints then
                        error(ertyp)
                     else
                        if n=10 then
                           emit0(pref)
                        else
                           emit0(sleap);
                     if sy=rparent then
                        insymbol
                     else
                     	error(errparent)
                     end
                  end;


         end (*case*)
      end (*standproc*);


      begin   (* Statement *)
         if sy in statbegsys+[ident] then
            case sy of
               ident:
                  begin
                  i:=loc(id); insymbol;
                  if i<>0 then
                     case tab[i].obj of
                        konstant:
                           error(ersym);
								type1:
									if tab[i].typ = procs then
										error(ervar)
									else
										error(ersym);

                        variable, address:
                           if tab[i].typ in [monvars,protvars] then
                              capscall(i)
                           else
                              if contains([procs],tab[i].typ,tab[i].ref) then
                                 if incobegin then
                                    call(fsys,i)
                                 else
                                    entrycall(fsys,i)
                              else
                                 assignment(tab[i].lev,tab[i].taddr);

                        prozedure,
								monproc,
								xgrdproc,
								grdproc:
									begin
									if tab[i].obj in [grdproc,xgrdproc] then
										if (curcaps <> 0) and (tab[curcaps].typ = protvars) then
											error(ergrdcall);
                           if tab[i].lev<>0 then
                              call(fsys,i)
                           else
                              standproc(tab[i].taddr)
									end ;

                        funktion:
                           if tab[i].ref=display[level] then
                              assignment(tab[i].lev+1,0)
                           else
                              error(ertyp) ;
                     end  (* case tab[i].obj of *)
                  end;  (* ident case *)

               beginsy:
                  if id = 'cobegin   ' then
                     begin
							if wascobegin or inaloop then
								error(ercob);
							incobegin := true;
                     if level = 1 then
								begin
                     	emit0(cobeg);
								wascobegin := true
								end
							else
								error(erlev);
                     compoundstatement;
							emit0(coend)
                     end
                  else
                     compoundstatement;

               ifsy:
                  ifstatement;

					casesy:
						casestatement;

               whilesy:
                  whilestatement;

               repeatsy:
                  repeatstatement;

               forsy:
                  forstatement;

               selectsy,prisy:
                  selstatement;


               nullsy:
                  insymbol;
               acceptsy:
                  acceptstatement;
					requeuesy:
						requeuestatement;

            end; (* case sy of *)
         test(fsys,[],ersemi)
      end;  (* statement *)






	procedure testforward(k: integer);

	(* test that forward declarations (forward, provides) were resolved *)

	var
		noerror: boolean;

	begin
		noerror := true;
		while (k <> 0) and noerror do
			begin
			with tab[k] do
			if typ = procs then
				begin
				if not normal then
					begin
					noerror := false;
					error(erprovdec)
					end
				end
			else
				if obj in [prozedure,funktion,monproc,grdproc,xgrdproc] then
					if not normal then
						begin
						noerror := false;
						error(erfordec)
						end;
			k := tab[k].link
			end
	end;  (* testforward *)





      procedure capsuledeclaration(form: types);

		(* process declaration of encapsulating objekts:
		   monitors or resources *)

      var
         lc2: integer;
			firstguard: integer;
			glc1: integer;
			prt, prb: integer;
            debug : integer;

         procedure exportlist;

            procedure entermp;

				(* enter procedure identifier in export table *)

            begin
					if sy <> ident then
						skip([ident,comma,semicolon],erident);
               if sy=ident then
                  begin
                  if ncapsprocs = maxcapsprocs then fatal(9);
                  ncapsprocs := ncapsprocs + 1;
                  capsproctab[ncapsprocs].name := id;
                  insymbol
                  end
            end;  (* entermp *)


         begin  (* Exportlist *)
            insymbol;
				if sy <> ident then
					skip([ident,comma,semicolon,exportsy],erident);
            while sy=ident do
               begin
               entermp;
               while sy=comma do
                  begin
                  insymbol; entermp
                  end;  (* while sy=comma *)
               if sy=semicolon then insymbol else error(ersemi)
            	end  (* while sy = ident *)
         end;  (* exportlist *)





         procedure checkdecs;

         (* ensure that all exported procedures have been declared *)

         var
            ok: boolean;
            i: integer;

         begin
            ok := true;
            for i := 1 to ncapsprocs do
               if not capsproctab[i].foundec then
                  ok := false;
            if not ok then
               error(ercapsprocdecs)
         end;  (* procedure checkdecs *)




			procedure guardedprocdec;
			var
				prb, prt: integer;
				lc1, lc2, lc3, lc4: integer;
				i: integer;
				x: item;
				qname: alfa;
				localdx: integer;
				qref: integer;
				nestedgrd: boolean;
				wasforward: boolean;
                debug : integer;


			begin  (* guardedprocdec *)
				wasforward := false;
				nestedgrd := inguardedproc;
				if tab[curcaps].typ <> protvars then
					error(eronlyinres)
				else
					if nestedgrd then
						error(ernotingrdproc);
				inguardedproc := true;
				insymbol;
				if sy = proceduresy then
					insymbol
				else
					skip(fsys,ersym);
         	if sy <> ident then
            	error(erident)
         	else
					begin
					i := searchblock(btab[display[level]].last,id);
					if (i = 0) or (tab[i].normal) then
						begin  (* no pending forward declaration *)
						if isexported then
							enter(id,xgrdproc)
						else
           				enter(id,grdproc);
						prt := t;
            		with tab[prt] do
               		begin
               		typ := notyp; ref := b + 1;
               		normal := true; lev := level;
               		end
               	end (* no pending forward declaration *)
					else
						begin
						insymbol;
						wasforward := true;
						level := level + 1
						end
					end;  (* sy was id *)
				if not wasforward then
					begin  (* no pending forward declaration *)
					internalname(internalnum,	qname);
					enter(qname,variable);
					with tab[t] do
						begin
						typ := protq;
						ref := 0;
						normal := true;
						lev := 1;
                        debug := taddr;
						alloc(intsize,dx,debug);
                        taddr := debug
						end;
					qref := t;
					insymbol;
					level := level + 1;
         		localdx := actrecsize;
         		if level>lmax  then fatal(5);
         		test([lparent,semicolon,whensy],fsys,ersym);
         		enterblock;display[level]:=b; prb:=b;
         		tab[prt].ref := prb;
         		if sy=lparent then
            		parameterlist(false,localdx);
         		align(localdx);
         		btab[prb].lastpar:= t; btab[prb].psize:=localdx;
					level := level - 1;
					if sy = whensy then insymbol else error(ersym);
					tab[prt].taddr := lc;
					if firstguard = -1 then
						begin
						firstguard := lc;
						tab[curcaps].auxref := firstguard
						end
					else
						code[glc1].y := lc;
					expression(fsys+[semicolon],x);
					lc1 := lc;
					emit0(prtjmp);
					(* process searching for a candidate *)
					lc2 := lc;
					emit0(jmpiz);
					(* guard open - load address of queue *)
					emit2(ldadr,tab[qref].lev,tab[qref].taddr);
					glc1 := lc;
					code[lc2].y := lc;
					emit0(jmp);
					(* process calling the procedure *)
					code[lc1].y := lc;
					lc3 := lc;
					emit0(jmpiz);
					lc4 := lc;
					emit0(jmp);
					code[lc3].y := lc;
					emit1(prtcnd,firstguard);
					emit2(ldadr,tab[qref].lev,tab[qref].taddr);
					emit0(prtslp);
					code[lc4].y := lc;
					level := level + 1;
					if x.typ <> bools then error(ertyp);
					end  (* no pending forward declaration *)
				else
					begin (* has been forward declared *)
					if not (tab[i].obj in [grdproc,xgrdproc]) then
						error(erdup);
					prt := i;
					prb := tab[prt].ref;
					localdx := btab[prb].vsize;
					tab[prt].normal := true;
					display[level] := prb;
					code[tab[prt].auxref].y := lc
					end;
				if sy = semicolon then insymbol else error(ersemi);
				if sy = forwardsy then
					begin
					insymbol;
					tab[prt].normal := false;
					tab[prt].auxref := lc;
					emit0(jmp);
					end
				else
					begin  (* this is not a forward declaration *)
					repeat
						while sy=constsy do
							constantdeclaration;
						while sy=typesy  do
							typedeclaration;
						while sy=varsy   do
							variabledeclaration(localdx);
						while sy=monitorsy do
							capsuledeclaration(monvars);
						while sy = resourcesy do
							capsuledeclaration(protvars);
						align(localdx);
						while sy in [proceduresy,functionsy] do
							procdeclaration;
						while sy = processsy do
							processdeclaration;
						while sy = guardedsy do
							guardedprocdec;
						test(blockbegsys,statbegsys,ersym)
					until not(sy in (blockbegsys - [beginsy]));
					testforward(btab[prb].last);
					insymbol;
					statement([semicolon,endsy]+fsys);
					while sy in [semicolon]+statbegsys do
						begin
						if sy=semicolon  then
							insymbol
						else
							error(ersemi);
						statement([semicolon,endsy]+fsys)
						end;
					emit0(retproc);
					if sy=endsy  then
						insymbol
					else
						error(erend);
					end;  (* this is not a forward declaration *)
				btab[prb].vsize:=localdx;
				level := level - 1;
				if sy = semicolon then insymbol else error(ersemi);
				inguardedproc := nestedgrd
			end;  (* guardedprocdeC *)


   begin  (* Capsuledeclaration *)
         if level <> 1 then error(erlev);
         initmons;
         insymbol;
         if sy <> ident then
            error(erident)
         else
            begin
            entervariable;
            enterblock;
				prt := t;
				prb := b;
				curcaps := prt;
            with tab[t] do
               begin
               typ := form;  ref := b;  normal := true;
               lev := level;
                    debug := taddr;
					if form = monvars then
						alloc(monvarsize,dx,debug)
					else
						alloc(protvarsize,dx,debug);
                    taddr := debug
               end
            end;  (* sy was ident *)
         if sy=semicolon then insymbol else error(ersemi);
         if level=lmax then fatal(5);
         level := level + 1;  codelevel := level;
			display[level] := b;
         if sy <> exportsy then error(erexport);
         while sy = exportsy do
            exportlist;
			firstguard := -1;
			repeat
         	while sy=constsy do
					constantdeclaration;
				while sy=typesy do
            	typedeclaration;
         	while sy=varsy do
            	variabledeclaration(dx);
         	while sy=monitorsy do
            	capsuledeclaration(monvars);  (* for error recovery only *)
         	while sy in [proceduresy,functionsy] do
            	procdeclaration;
				while sy = processsy do
					processdeclaration;  (* for error recovery only *)
				while sy = guardedsy do
					guardedprocdec
			until not(sy in (blockbegsys-[beginsy]));
         checkdecs;
			if firstguard <> -1 then
				code[glc1].y := lc
			else
				tab[curcaps].auxref := lc;;
			emit0(prtsel);
         lc2 := lc;  (* start of capsule body code *)
			testforward(btab[prb].last);
         if sy=beginsy then
            statement([semicolon,endsy]+fsys)
         else
            if sy=endsy then insymbol else error(erend);
         if lc2 <> lc then
            begin
            with montab do
					if n = maxmons then
						fatal(14)
					else
               	begin
               	n := n + 1;
               	startadds[n] := lc2
               	end;
            	emit0(mretn)
            	end;
         testsemicolon;
         level := level -1;
			codelevel := level;
         curcaps := 0
      end;  (* capsuledeclaratioN *)


      procedure entrydecs;

      (* parse process entry declarations *)

      var
            prdx: integer;
            debug : integer;

      begin
         while sy = entrysy do
            begin
            insymbol;
            if sy = ident then
               begin
               entervariable;
               enterblock;
               with tab[t] do
                  begin
                  typ := entrys;
                  ref := b;
                  lev := level;
                  normal := true;
						getmapping(constbegsys);
                        debug := taddr;
						if obj = address then
							enterint(t,entrysize,dx,debug)
						else
							alloc(entrysize,dx,debug);
                        taddr := debug;
						prdx := dx
                  end;  (* with tab[t] *)
               if sy = lparent then
                  begin
                  level := level + 1;
                  display[level] := b;
                  parameterlist(true,dx);
                  level := level - 1;
						align(dx)
                  end;
               btab[b].lastpar := t;
               btab[b].psize := dx - prdx;
               if sy = semicolon then insymbol else error(ersemi);
               end  (* sy was ident *)
            else
               error(erident)
            end  (* while sy = entrysy *)
      end;  (* entrydecs *)

		procedure entrymap(t: integer);

		var
			index: integer;
			found: boolean;

		begin
			while t <> 0 do
				begin
				if (tab[t].obj = address) and (tab[t].typ = entrys) then
					begin
					index := 1;
					found := false;
					repeat
						if intab[index].tabref = t then
							found := true
						else
							index := index + 1
					until found;
					emit1(enmap,index-1)
					end;
				t := tab[t].link
				end  (* while *)
		end;  (* entrymap *)


 begin  (* Block *)
		codelevel := level;
      if tab[prt].normal then
         begin  (* was not forward declared *)
         dx := actrecsize;
         if level>lmax  then fatal(5);
         test([lparent,colon,semicolon,providessy],fsys,ersym);
         enterblock;display[level]:=b; prb:=b;
         if level = 1 then
            tab[prt].typ := notyp;
         tab[prt].ref := prb;
         if (sy=lparent) and (level>1)  then
            parameterlist(false,dx);
         align(dx);
         btab[prb].lastpar:=t; btab[prb].psize:=dx;
         if lobj = funktion then
            begin  (* function *)
            if sy=colon   then
               begin  (* get function type *)
               insymbol;
               if sy=ident then
                  begin
                  x:=loc(id); insymbol;
                  if x <> 0 then
                     if tab[x].obj <> type1 then
                        error(ertyp)
                     else
                        if tab[x].typ in (stantyps+[enums])  then
									begin
                           tab[prt].typ:=tab[x].typ;
									if tab[x].typ = enums then
										tab[prt].auxref := tab[x].auxref
									end
                        else
                           error(ertyp)
                  end
               else
                  skip([semicolon]+fsys,erident)
               end   (* get function type *)
            else
               error(ercolon);
            end  (* function *)
         end  (* not forward declared *)
      else
         begin  (* was forward declared *)
         prb := tab[prt].ref;
         dx := btab[prb].vsize;
         display[level]:= prb;
         if tab[prt].typ = procs then
            begin
            parametercheck(prt);
            if sy = semicolon then insymbol else error(ersemi);
            entrycheck(prt)
            end
         end;  (* was forward decalred *)
      if sy = providessy then
         begin
         insymbol;
         tab[prt].normal := false;
         entrydecs;
         btab[tab[prt].ref].vsize := dx;
         if sy = endsy then insymbol else error(erend)
         end
      else
         begin  (* not providessy *)
         if sy=semicolon  then
            insymbol ;
         if sy = forwardsy then
            begin  (* forward declaration *)
            insymbol;
            if level =  1 then error(ersym);
            if not tab[prt].normal then error(ersym);
            tab[prt].normal := false;
            btab[tab[prt].ref].vsize := btab[tab[prt].ref].psize
	         end  (* forward declaration *)
         else
            begin  (* not forwardsy *)
            if sy = entrysy then
               entrydecs;
            tab[prt].normal := true;
            if level=1 then
                begin
                enter('any       ',variable);
                with tab[t] do
						begin
                     typ := synchros;
                     normal := true;
                            debug := taddr;
							alloc(synchrosize,dx,debug);
                            taddr := debug
                     end  (* with *)
                  end;  (* if  level=1 *)
               repeat
                   while sy=constsy do
                       constantdeclaration;
                 while sy=typesy  do
                  typedeclaration;
                   while sy=varsy   do
                    variabledeclaration(dx);
                 while sy=monitorsy do
                     capsuledeclaration(monvars);
						while sy = resourcesy do
							capsuledeclaration(protvars);
                  align(dx);
                 while sy in [proceduresy,functionsy,guardedsy] do
							begin
							if sy = guardedsy then
								begin
								error(eronlyinres);
								insymbol
								end;
                     procdeclaration
							end;
                  while sy = processsy do
                     processdeclaration;
                 btab[prb].vsize:=dx;
                 test(blockbegsys,statbegsys,ersym)
                until not(sy in (blockbegsys - [beginsy]));

               tab[prt].taddr:=lc;

              if level = 1 then
                  with montab do
                   for ttt := 1 to n do
                      emit1(mexec,startadds[ttt]);
					if tab[prt].typ = procs then
						entrymap(btab[tab[prt].ref].last);
					testforward(btab[prb].last);
               insymbol;
					statement([semicolon,endsy]+fsys);
               while sy in [semicolon]+statbegsys do
               	begin
               	if sy=semicolon  then
                  	insymbol
                  else
                  	error(ersemi);
               	statement([semicolon,endsy]+fsys)
                  end;
               if sy=endsy  then
                 insymbol
               else
                 error(erend);
               end  (* not forward *)
         end;  (* not providessy *)
            test(fsys+[period],[],ersym);
   end;   (* blocK *)

   var
    S:string;
    I:integer;


      begin  (* Pfcfront *)
         {s:=Memo1.Lines;}
         Done:=false;
         writeln(listfile); writeln(listfile);
         headermsg(output);
         writeln(listfile);


         {reset(progfile);}



         (* the compiler listing is sent to listfile *)


         {rewrite(listfile);}
         headermsg(listfile);
         write(listfile, 'Compiler listing');

         writeln(listfile);
         writeln(listfile);

         initkeytab;

         sps['+']:= plus;                   sps['-']:=minus;
		     sps['/'] := rdiv;
         sps['(']:= lparent;                sps[')']:=rparent;
         sps['=']:= eql;                    sps[',']:=comma;
         sps['[']:= lbrack;                 sps[']']:=rbrack;
         sps['"']:= neq;                    sps['&']:=andsy;
         sps[';']:= semicolon;              sps['*']:=times;
         sps['!'] := shriek;                sps['?'] := query;
		     sps['%'] := percent;

         legalchars := ['A'..'Z','a'..'z','0'..'9',
								':','<','>','.','(','''',
                        '{','=','+','-','*','/',')','}',',','[',']',
                        ';','?','!','%'];
         constbegsys := [plus,minus,intcon,realcon,charcon,ident];
         typebegsys  := [ident,arraysy,recordsy,channelsy,lparent];
         blockbegsys := [constsy,typesy,varsy,
            monitorsy,proceduresy,functionsy,processsy,beginsy,
				resourcesy,guardedsy];
         facbegsys   := [intcon,realcon,charcon,ident,lparent,notsy,lbrack];
         statbegsys  := [beginsy,ifsy,casesy,whilesy,repeatsy,forsy,
         selectsy,prisy,nullsy,acceptsy,requeuesy];
         stantyps    := [notyp,ints,reals,bools,chars];
         simpletyps := stantyps + [enums,bitsets];
			ipctyps := [semafors, condvars, channels, entrys];

         lc:=0; ll:=0; cc:=0; ch:=' ';
			linenum := 0;
			lineold := 0; linenew := 0;
         errpos:=0; errs:=[]; insymbol;
         t:=-1; a:=0; b:=1; sx:=0;  chan := 0;
			r := 0;
         display[0]:=1;
         skipflag:=false;
         montab.n := 0;
         initmons;
         int := 0;
			et := 0;
			incobegin := false; wascobegin := false;
			inprocessdec := false;
			inaloop := false;
			labelnum := 0;
			internalnum := 0;

         if sy <> programsy  then
            error(erprogram)
         else
            begin
            insymbol;
            if sy <> ident  then
               error(erident)
            else
               begin
               progname:=id; insymbol;
               end
            end;
         writeln('Compiling ',string (progname),' ...');


         enter('          ',       variable,notyp,0); (*sentinel*)
			   enter('maxint    ',			konstant,ints,intmax);
         enter('false     ',       konstant,bools,fals);
         enter('true      ',       konstant,bools,tru);
         enter('char      ',       type1,chars,charsize);
         enter('boolean   ',       type1,bools,boolsize);
         enter('integer   ',       type1,ints,intsize);
			   enter('real      ',			type1,reals,realsize);
         enter('semaphore ',       type1,semafors,semasize);
         enter('condition ',        type1,condvars,condvarsize);
         enter('synchronou',        type1,synchros,synchrosize);
         enter('bitset    ',        type1,bitsets,bitsetsize);

		     enter('abs       ',			funktion,notyp,0);
		     enter('sqr       ',			funktion,notyp,2);
		     enter('odd       ',			funktion,bools,4);
	       enter('chr       ',			funktion,chars,5);
		     enter('ord       ',			funktion,ints,6);
		     enter('succ      ',			funktion,notyp,7);
		    enter('pred      ',			funktion,notyp,8);
		  	enter('round     ',			funktion,ints,9);
		   enter('trunc     ',			funktion,ints,10);
		   	enter('sin       ',			funktion,reals,11);
			  enter('cos       ',			funktion,reals,12);
			  enter('exp       ',			funktion,reals,13);
			  enter('ln        ',			funktion,reals,14);
			  enter('sqrt      ',			funktion,reals,15);
			  enter('arctan    ',			funktion,reals,16);
         enter('eof       ',       funktion,bools,17);
         enter('eoln      ',       funktion,bools,18);
         enter('random    ',        funktion,ints,19);
         enter('empty     ',        funktion,bools,20);
		     enter('bits      ',			funktion,bitsets,21);
         enter('int       ',        funktion,ints,24);
         enter('clock     ',        funktion,ints,25);
         enter('read      ',       prozedure,notyp,1);
         enter('readln    ',       prozedure,notyp,2);
         enter('write     ',       prozedure,notyp,3);
         enter('writeln   ',       prozedure,notyp,4);
         enter('wait      ',       prozedure,notyp,5);
         enter('signal    ',       prozedure,notyp,6);
         enter('delay     ',        prozedure,notyp,7);
         enter('resume    ',        prozedure,notyp,8);
         enter('initial   ',        prozedure,notyp,9);
         enter('priority  ',        prozedure,notyp,10);
         enter('sleep     ',        prozedure,notyp,11);

         enter('_main     ',       prozedure,notyp,0);

			useridstart := t;

         with btab[1] do
            begin
            last:=t; lastpar:=1; psize:=0; vsize:=0;
            end;
         block(blockbegsys+statbegsys,prozedure,t,1);
         if sy<>period then
            error(erperiod);
         emit0(stop);
         if errs = []  then
            begin
				success := true;
            writeln('Compilation complete')
            end
         else
				begin
				success := false;
            errormsg
				end;
         99:writeln;
end;  (* pfcfront *)

	(* @(#)listings.i	4.4 11/8/91 *)

	procedure putsuff(anytype: types);

			(* write suffix in "assembly" listing *)

         begin
				if anytype in stantyps+[bitsets,adrs,enums] then
            case anytype of
					notyp: write(listfile,'    ');
               ints: write(listfile,'.i  ');
               bools: write(listfile,'.b  ');
               chars: write(listfile,'.c  ');
					reals: write(listfile,'.r  ');
					adrs: write(listfile,'.adr');
					enums: write(listfile,'.enm');
					bitsets: write(listfile,'.bs ');

            end  (* case *)
				else
					write(listfile,'    ')
         end;  (* procedure putsuff *)


		procedure putop(fop: opcode; var tofile: text);

		(* write op-code to standard output *)

		begin  (* Putop *)
			case fop of
				ldadr:		write(tofile,'ldadr');
				ldval:		write(tofile,'ldval');
				ldind:		write(tofile,'ldind');
				updis:		write(tofile,'updis');
				cobeg:		write(tofile,'cobeg');
				coend:		write(tofile,'coend');
				wait:			write(tofile,'swait');
				signal:		write(tofile,'signl');
				stfun:		write(tofile,'stfun');
				ixrec:		write(tofile,'ixrec');
				jmp:			write(tofile,'jmpuc');
				jmpiz:		write(tofile,'jmpiz');
				for1up:		write(tofile,'for1u');
				for2up:		write(tofile,'for2u');
				mrkstk:		write(tofile,'mkstk');
				callsub:		write(tofile,'calls');
				ixary:		write(tofile,'ixary');
				ldblk:		write(tofile,'ldblk');
				cpblk:		write(tofile,'cpblk');
				ldcon:		write(tofile,'ldcon');
				ifloat:		write(tofile,'float');
				readip:		write(tofile,'rdinp');
				wrstr:		write(tofile,'wrstr');
				wrval:		write(tofile,'wrval');
				wrbas:		write(tofile,'wrbas');
				stop:			write(tofile,'stopx');
				retproc:		write(tofile,'rproc');
				retfun:		write(tofile,'rfunc');
				repadr:		write(tofile,'rpadr');
				notop:		write(tofile,'notop');
				negate:		write(tofile,'negat');
				store:		write(tofile,'store');
				relequ:		write(tofile,'releq');
				relneq:		write(tofile,'relne');
				rellt:		write(tofile,'rellt');
				relle:		write(tofile,'relle');
				relgt:		write(tofile,'relgt');
				relge:		write(tofile,'relge');
				orop:			write(tofile,'iorop');
				add:			write(tofile,'addop');
				sub:			write(tofile,'subop');
				andop:		write(tofile,'andop');
				mul:			write(tofile,'mulop');
				divop:		write(tofile,'divop');
				modop:		write(tofile,'modop');
				rdlin:			write(tofile,'rdlin');
				wrlin:			write(tofile,'wrlin');
				selec0:		write(tofile,'sel0 ');
				chanwr:		write(tofile,'chnwr');
				chanrd:		write(tofile,'chnrd');
				delay:		write(tofile,'delay');
				resum:		write(tofile,'resum');
				enmon:	write(tofile,'enmon');
				exmon:		write(tofile,'exmon');
				mexec:		write(tofile,'mexec');
				mretn:		write(tofile,'mretn');
				lobnd:		write(tofile,'lobnd');
				hibnd:		write(tofile,'hibnd');
				pref:			write(tofile,'prefr');
				sleap:		write(tofile,'sleep');
				procv:		write(tofile,'procv');
				ecall:		write(tofile,'ecall');
				acpt1:		write(tofile,'acpt1');
				acpt2:		write(tofile,'acpt2');
				rep1c:		write(tofile,'rep1c');
				rep2c:		write(tofile,'rep2c');
				btest:		write(tofile,'btest');
				enmap:		write(tofile,'enmap');
				wrfrm:		write(tofile,'wrfrm');
				w2frm:		write(tofile,'w2frm');
				wrsfm:		write(tofile,'wrsfm');
				power2:		write(tofile,'powr2');
				slabl:		write(tofile,'slabl');
				blokk:		write(tofile,'block');
				param:		write(tofile,'param');
				case1:		write(tofile,'case1');
				case2:		write(tofile,'case2');
				selec1:		write(tofile,'sel1 ');

				sinit:		write(tofile,'sinit');
				prtex:		write(tofile,'prxit');
				prtjmp:		write(tofile,'prjmp');
				prtsel:		write(tofile,'prsel');
				prtslp:		write(tofile,'prslp');
				prtcnd:		write(tofile,'prcnd');
			end  (* case *)
		end;  (* putop *)


         procedure writetype(anytype: types);

         begin
            case anytype of
               notyp: 		write(listfile,'notyp       ');
					bitsets: 	write(listfile,'bitset      ');
               ints: 		write(listfile,'integer     ');
					reals:		write(listfile,'real        ');
               bools: 		write(listfile,'boolean     ');
               chars: 		write(listfile,'char        ');
               arrays: 		write(listfile,'array       ');
               records: 	write(listfile,'record      ');
               semafors: 	write(listfile,'semaphore   ');
               channels: 	write(listfile,'channel     ');
               monvars:  	write(listfile,'monvar      ');
					protvars:	write(listfile,'resource    ');
					protq:		write(listfile,'protq       ');
               condvars:  	write(listfile,'condition   ');
               synchros:  	write(listfile,'synch       ');
					adrs: 		write(listfile,'address     ');
               procs:   	write(listfile,'process     ');
               entrys:  	write(listfile,'entry       ');

					enums:		write(listfile,'enum type   ')

            end  (* case *)
         end;  (* procedure writetype *)



         procedure writeobj(anyobj: objekt);

         begin
            case anyobj of
               konstant: write(listfile,'constant    ');
               variable: write(listfile,'variable    ');
               type1: write(listfile,'type id     ');
               prozedure: write(listfile,'procedure   ');
               funktion: write(listfile,'function    ');
               monproc:  write(listfile,'monproc     ');
					address:  write(listfile,'address     ');
					grdproc:  write(listfile,'grdproc     ');
					xgrdproc: write(listfile,'xgrdproc    ')
            end (* case *)
         end;  (* procedure writeobj *)



      procedure puttab;

      (* send symbol table to listfile *)

      var
         index: integer;








         procedure putfulltab;

         (* output full symbol table *)

         begin  (* putfulltab *)
				index := useridstart;
            writeln(listfile); writeln(listfile);
            writeln(listfile, 'Symbol table');
            writeln(listfile);
            write(listfile,'    ','name      ',' link','      objekt','       type ',
      '      ','  ref', '      nrm','  lev','  adr','  aux');
               writeln(listfile); writeln(listfile);
               while index <= t do
                  begin
                  write(listfile,index:3,' ');
                  with tab[index] do
                     begin
                     write(listfile,string(name));
                  write(listfile,link:5,'     ');
                  writeobj(obj);
                  writetype(typ);
                  write(listfile,ref:5,'     ');
                  write(listfile,normal:5);
                  write(listfile,lev:5);
                  writeln(listfile,taddr:5,auxref:5)
                  end;  (* with *)
               index := index + 1
               end
      end;  (* putfull tab *)





      procedure putcode;

      (* output pcode to listfile *)

      var
         local: 0..cmax;

      begin
         writeln(listfile);
         writeln(listfile,'Generated P-code');
         writeln(listfile);
         for local := 0 to lc - 1 do
				with code[local] do
					begin
					write(listfile,local:5,'     ');
					putop(f,listfile);
					putsuff(instyp);
					writeln(listfile,x:5,y:10,'          ;',line:1)
					end
      end;  (* putcode *)


      begin  (* Puttab *)
         index := 1;
            putfulltab;
            putcode
      end;  (* puttaB *)


(* implementation-checking procedure *)


	(* @(#)impcheck.i	4.1 10/24/89 *)

	procedure impcheck(var success: boolean);

	(* check generated code ofr use of features not in the
	   implementation *)

	const
		ni = ' not implemented';

	var
		index: integer;


	begin (* Impcheck *)
		writeln(listfile);
		for index := useridstart to t do
			with tab[index] do
				if obj = address then
					begin
					if not impmapping then
						begin
						writeln(listfile,'e - ',string(name),' address mapping',ni);
						success := false
						end
					end
				else
					if typ = reals then
						if not impreals then
							begin
							writeln(listfile,'e - ',string(name),' reals',ni);
							success := false
							end;
		if r <> 0 then
			if not impreals then
				writeln(listfile,'e - real literals used in program')
	end;  (* impchecK *)



(* intermediate code translator procedure *)

procedure ict(var success: boolean);


(* Pascal-FC intermediate code translator for Unix systems *)




      procedure putcode;

      (* outputs the objektcode array *)

      var
         cindex: integer;




			procedure gen(fobj, xobj, yobj: integer);
      var J,K:integer;

			begin


      with objfile do
					with gencode[cindex] do
						begin
						f := fobj;
						x := xobj;
						y := yobj;
						l := code[cindex].line
						end
			end;  (* gen *)





      begin  (* Putcode *)
         	for cindex := 0 to lc - 1 do
            	with code[cindex] do
						case f of
							ldadr:	gen(0,x,y);
							ldval: 	gen(1,x,y);
							ldind: 	gen(2,x,y);
							updis:	gen(3,x,y);
							cobeg:	gen(4,x,y);
							coend:	gen(5,x,y);
							wait:		gen(6,x,y);
							signal:	gen(7,x,y);
							stfun:	gen(8,x,y);
							ixrec:	gen(9,x,y);
							jmp:		gen(10,0,y);
							jmpiz:	gen(11,0,y);
							case1:	gen(12,0,y);
							case2:	gen(13,0,0);
							for1up:	gen(14,0,y);
							for2up:	gen(15,0,y);
							mrkstk:	gen(18,x,y);
							callsub:	gen(19,x,y);
							ixary:	gen(21,x,y);
							ldblk:	gen(22,x,y);
							cpblk:	gen(23,x,y);
							ldcon:	if instyp = reals then
											gen(25,0,y)
										else
											gen(24,0,y);
							ifloat:	gen(26,0,y);
							readip:	case instyp of
											notyp,
											ints:		gen(27,0,1);
											reals:	gen(27,0,4);
											chars:	gen(27,0,3)
										end;
							wrstr:	gen(28,0,y);
							wrsfm:	gen(28,1,y);
							wrval:	case instyp of
											notyp,
											ints,
											semafors:		gen(29,0,1);
											bools:	gen(29,0,2);
											chars:	gen(29,0,3);
											reals:	gen(29,0,4);
											bitsets:	gen(29,0,5)
										end;
							wrfrm:	case instyp of
											notyp,
											ints,
											semafors:		gen(30,0,1);
											bools:	gen(30,0,2);
											chars:	gen(30,0,3);
											reals:	gen(30,0,4);
											bitsets:	gen(30,0,5)
										end;
							w2frm:	gen(37,0,0);
							wrbas:	if instyp = ints then
											gen(107,0,1)
										else
											gen(107,0,5);
							stop:		gen(31,x,y);
							retproc:	gen(32,x,y);
							retfun:	gen(33,x,y);
							repadr:	gen(34,x,y);
							notop:	gen(35,x,y);
							negate:	gen(36,x,y);
							store:	gen(38,0,0);
							relequ:	case instyp of
											notyp,
											ints,
											bools,
											chars,
											enums:	gen(45,0,0);
											reals:	gen(39,0,0);
											bitsets:	gen(112,0,0)
										end;
							relneq:	case instyp of
											notyp,
											ints,
											bools,
											chars,
											enums:	gen(46,0,0);
											reals:	gen(40,0,0);
											bitsets:	gen(113,0,0)
										end;
							rellt:	case instyp of
											notyp,
											ints,
											bools,
											chars,
											enums:	gen(47,0,0);
											reals:	gen(41,0,0);
											bitsets:	gen(114,0,0)
										end;
							relle:	case instyp of
											notyp,
											ints,
											bools,
											chars,
											enums:	gen(48,0,0);
											reals:	gen(42,0,0);
											bitsets:	gen(115,0,0)
										end;
							relgt:	case instyp of
											notyp,
											ints,
											bools,
											chars,
											enums:	gen(49,0,0);
											reals:	gen(43,0,0);
											bitsets:	gen(116,0,0)
										end;
							relge: 	case instyp of
											notyp,
											ints,
											bools,
											chars,
											enums:	gen(50,0,0);
											reals:	gen(44,0,0);
											bitsets:	gen(117,0,0)
										end;
							orop:		if instyp = bools then
												gen(51,0,0)
										else
											gen(118,0,0);
							add:		if instyp = ints then
											gen(52,0,0)
										else
											gen(54,0,0);
							sub:		if instyp = ints then
											gen(53,0,0)
										else
											if instyp = reals then
												gen(55,0,0)
											else
												gen(119,0,0);
							andop:	if instyp = bools then
											gen(56,0,0)
										else
											gen(120,0,0);
							mul:		if instyp = ints then
											gen(57,0,0)
										else
											gen(60,0,0);
							divop:	if instyp = ints then
											gen(58,0,0)
										else
											gen(61,0,0);
							modop:	gen(59,0,0);
							rdlin:	gen(62,0,0);
							wrlin:	gen(63,0,0);
							selec0:	gen(64,x,y);
							selec1:
										case x of
											0:		gen(24,0,-1);
											3,
											4,
											5:		gen(24,0,y)
										end;
							chanwr:	if instyp in [ints,bools,chars,reals,
															enums,bitsets] then
											gen(65,0,y)
										else
											gen(65,1,y);
							chanrd:	gen(66,0,y);
							delay:	gen(67,x,y);
							resum:	gen(68,x,y);
							enmon:	gen(69,x,y);
							exmon:	gen(70,x,y);
							mexec:	gen(71,x,y);
							mretn:	gen(72,x,y);
							lobnd:
										gen(74,0,y);
							hibnd:
										gen(75,0,y);
							slabl,
							blokk,
							param:	gen(78,0,0);
							pref:		begin
										gen(96,0,0);
										writeln('w - priorities not implemented')
										end;
							sleap:	gen(97,0,0);
							procv:	gen(98,0,0);
							ecall:	gen(99,0,y);
							acpt1:	gen(100,0,y);
							acpt2:	gen(101,0,y);
							rep1c:	gen(102,x,y);
							rep2c: 	gen(103,0,y);
							power2:	gen(104,0,0);
							btest:	gen(105,0,0);
							enmap:	gen(106,0,y);
							sinit:	gen(121,0,0);
							prtjmp:	gen(129,0,y);
							prtsel: 	gen(130,0,0);
							prtslp:	gen(131,0,0);
							prtex:	gen(132,x,0);
							prtcnd:	gen(133,0,y)
						end;  (* case *);
			objfile.ngencode := lc - 1
      end;  (* putcode *)


		procedure puttabs;

		(* output tables, etc, to objfile *)

		begin
     with objfile do
     begin
			fname := filename;
			prgname := progname;
		  gentab := tab;
			ngentab := t;
			genatab := atab;
			ngenatab := a;
			genbtab := btab;
		  ngenbtab := b;
			genstab := stab;
			genrconst := rconst;
			ngenstab := sx;
			useridstart := useridstart
     end
		end;  (* puttabs *)



begin  (* Ict *)
	(* implementation checks to go here *)
	if success then
		begin
		putcode;
    puttabs;
    end
end;  (* ict *)

	procedure errorbanner;

	begin
		writeln('*********************************');
		writeln('Compilation errors - see listfile');
		writeln('*********************************')
	end;





procedure TForm1.Button1Click(Sender: TObject);
begin
  FindDialog1.Execute;
end;

procedure TForm1.Copy1Click(Sender: TObject);
begin
   if Memo1.Focused then
    Memo1.CopyToClipboard;
 end;

procedure TForm1.Cut1Click(Sender: TObject);
begin
    if Memo1.Focused then
    Memo1.CutToClipboard;

end;

procedure TForm1.Exit1Click(Sender: TObject);
begin
 Close;
end;

procedure TForm1.FindDialogFind(Sender: TObject);
var
 startpos : integer;
 S:string;
 { mySearchTypes : TSearchTypes; }
begin

   with TFindDialog(Sender) do
  begin
    {If the stored position is 0 this cannot be a find next. }
    if FSelPos = 0 then
      Options := Options - [frFindNext];

     { Figure out where to start the search and get the corresponding
       text from the memo. }
    if frfindNext in Options then
    begin
      { This is a find next, start after the end of the last found word. }
      StartPos := FSelPos + Length(Findtext);
      S := Copy(Memo1.Lines.Text, StartPos, MaxInt);
    end
    else
    begin
      { This is a find first, start at the, well, start. }
      S := Memo1.Lines.Text;
      StartPos := 1;
    end;
    { Perform a global case-sensitive search for FindText in S }
    FSelPos := Pos(FindText, S);
    if FSelPos > 0 then
    begin
       { Found something, correct position for the location of the start
         of search. }
      FSelPos := FSelPos + StartPos - 1;
      Memo1.SelStart := FSelPos - 1;
      Memo1.SelLength := Length(FindText);
      Memo1.SetFocus;
    end
    else
    begin
      { No joy, show a message. }
      if frfindNext in Options then
        S := Concat('There are no further occurences of "', FindText,
          '" in Memo1.')
      else
        S := Concat('Could not find "', FindText, '" in Memo1.');
      MessageDlg('Text not found', mtInformation, [mbOK], 0);
    end;
  end;
end;

procedure GoToLineNumber(Memo: TMemo; LineNumber: Integer);
var
  LineStart: Integer;
begin
  // Check if the line number is within a valid range
  if (LineNumber >= 1) and (LineNumber <= Memo.Lines.Count) then
  begin
    // Calculate the character position at the start of the specified line
    LineStart := Memo.Perform(EM_LINEINDEX, LineNumber - 1, 0);

    // Set the SelStart property to the calculated position
    Memo.SelStart := LineStart;
  end;
end;

procedure TForm1.GoTo1Click(Sender: TObject);
var
 LineNumber: integer;
 S:string;
begin
     {Form2.Visible:=true; }
     if Form2.ShowModal = mrOK then
      try
        begin
        S:=Form2.Edit1.Text;
        LineNumber:=StrToInt(S);
        GoToLineNumber(Memo1, LineNumber);
      end;
        finally
          Form2.Free;
       end;

end;

procedure TForm1.Find1Click(Sender: TObject);
begin
  { ReplaceDialog1.CloseDialog; }
   FindDialog1.Execute;
end;


procedure TForm1.New1Click(Sender: TObject);
begin
   Memo1.Lines.Clear;
   MainMenu1.Items[1].Items[0].Enabled:=false;
   MainMenu1.Items[1].Items[1].Enabled:=false;
   MainMenu1.Items[1].Items[2].Enabled:=false;
   MainMenu1.Items[2].Items[0].Enabled:=false;
   MainMenu1.Items[3].Items[0].Enabled:=false;
 end;

procedure TForm1.Open1Click(Sender: TObject);
begin
   If OpenDialog1.Execute then
   begin
   NazevSouboru:=OpenDialog1.FileName;
   Memo1.Lines.LoadFromFile(NazevSouboru);
   MainMenu1.Items[1].Items[0].Enabled:=true;
   MainMenu1.Items[1].Items[1].Enabled:=true;
   MainMenu1.Items[1].Items[2].Enabled:=true;
   MainMenu1.Items[1].Items[3].Enabled:=true;
   MainMenu1.Items[2].Items[0].Enabled:=true;

   end;
end;

procedure TForm1.Paste1Click(Sender: TObject);
begin
  if Memo1.Focused then
    Memo1.PasteFromClipboard;

end;

procedure TForm1.Print1Click(Sender: TObject);
begin
  PrintDialog1.Execute;
end;

procedure TForm1.PrintSetup1Click(Sender: TObject);
begin
 PrinterSetupDialog1.Execute;
end;

procedure TForm1.ranslate2Click(Sender: TObject);
var
 NazevSouboru2:string;

begin
  AssignFile(progfile, NazevSouboru);
  reset(progfile);
  assignFile(listfile, ChangeFileExt(NazevSouboru,'.lst'));
  rewrite(listfile);
  Directory:= ExtractFileDir(NazevSouboru);
  NazevSouboru2:=  ChangeFileExt(NazevSouboru, '.out');
  assignFile(output, NazevSouboru2);
  rewrite(output);
  SetLength(obj, 1);
  Source:=Memo1.Text;
  LengthOfSource:=  Length(Source);
  PositionOfSource:=1;
  SetLength (Source, LengthOfSource);
  FileStream :=
   TFileStream.Create(ChangeFileExt(NazevSouboru,'.obj'),
      fmCreate);
  pfcfront(success);
	impcheck(success);
	if success then
		ict(success);
	puttab;
   try
    // Zápis pole objfile do souboru
    FileStream.WriteBuffer(objfile,  SizeOf(objcoderec));
  finally
    FileStream.Free;
   end;
  closeFile (progfile);
  closeFile(listfile);
  closeFile(output);
  MessageDlg('Done', mtInformation, [mbOK], 0);
  MainMenu1.Items[3].Items[0].Enabled:=true;
	if not success then
		errorbanner

end;

procedure Pint;

 (* Pascal-FC interpreter *)

const

(* implementation-independent constants *)
	(* @(#)globcons.i	4.1 10/24/89 *)

   alng=10; 		(* length of identifiers *)
   xmax = maxint;
   omax=200;            (* largest op-code for p-machine *)
   funmax = omax;	(* highest function number *)


(* implementation-dependent constants *)
(* impcons.i *)
(* BM 1 version *)


	target = 'IBM PC compatibles';

   maxmons=10;          (* maximum monitor in a program *)
   maxcapsprocs=10;     (* maximum exported procedures from a monitor *)
   intermax=10;         (* max no. of mapped ipc primitives *)
   tmax=150;            (* max size of symbol table *)
   bmax=50;             (* max size of block table *)
   amax=20;             (* max size of array table *)
   casemax=20;          (* max number of case labels or selects *)
   chanmax=20;          (* maximum size of channel table - gld *)
   cmax=2000;           (* max size of p-code array *)
   lmax=7;              (* max depth of block nesting *)
   smax=1500;           (* max size of string table *)
   rmax = 50;		(* real constant table limit *)
   etmax = 20;		(* enumeration type upper bounds table *)

   llng=121;		(* max source input line length *)
   tabstop=3;           (* for 1 implementation - gld *)
   tabchar = 9;

   fals = 0;
   tru = 1;
   charl=0;      	(* first legal ascii character *)
   charh=127;   	(* last legal ascii character *)

   intmax = 32767;	(* maximum integer on target *)
   intmsb = 16;		(* most sig. bit in target integer *)

   realmax = 1e38;	(* maximum real number on target
										   or host, whichever is smaller *)
   minreal = 1e-37;	(* smallest real (for division) *)
   emax = 38;		(* maximum real exponent on target *)
   emin = -emax;

   bsmsb = 7;		(* most sig. bit in target bitset *)

   impfiles = false;
   impmapping = false;
   imptiming = false;
   impreals = true;

   monvarsize = 2;
   protvarsize = 3;
   chansize=3;
   entrysize = 3;       (* space for a process entry point *)
   sfsize=6;            (* size of "frame" in a select statement *)

   bitsetsize = 1;
   intsize = 1;
   boolsize = 1;
   charsize = 1;
   semasize = 1;
   condvarsize = 1;
   synchrosize = 0;
   procsize = 1;
   enumsize = 1;
   realsize = 1;

   objalign = 1;
   pushdown = false;

(* interpreter-specific constants *)

   stepmax=8;
   statmax=200000;      (* maximum statements before "livelock *)

(* NOTE - make (stmax - (stkincr * pmax)) >= stkincr *)

   stmax=5000;
   stkincr=200;
   pmax=20;
   msb = 7;


   actrecsize = 5;	(* size of subprogram "housekeeping" block *)


type

	(* @(#)globtypes.i	4.7 11/8/91 *)

   opcode=(ldadr,ldval,ldind,updis,cobeg,coend,wait,signal,stfun,ixrec,
      jmp,jmpiz,for1up,for2up,mrkstk,callsub,ixary,ldblk,cpblk,
      ldcon,ifloat,readip,wrstr,wrval,stop,retproc,retfun,repadr,notop,
      negate,store,relequ,relneq,rellt,relle,relgt,relge,orop,
      add,sub,andop,mul,divop,modop,rdlin,wrlin,selec0,chanwr,
      chanrd,delay,resum,enmon,exmon,mexec,mretn,
      lobnd,hibnd,pref,sleap,
      procv,ecall,acpt1,acpt2,rep1c,rep2c,btest,enmap,wrfrm,w2frm,
      wrsfm,wrbas,power2,slabl,blokk,param,case1,case2,selec1,
      sinit,prtex,prtjmp,prtsel,prtslp,prtcnd);

   index = -xmax .. xmax;
   alfa = packed array[1..alng] of char;
   myobject = (konstant,variable,type1,prozedure,funktion,monproc,address,
             grdproc,xgrdproc);

   types = (notyp,ints,reals,bools,chars,arrays,records,
      semafors,channels,monvars,condvars,synchros,adrs,
      procs,entrys,enums,bitsets,
      protvars,protq);

   typset = set of types;

   fnametype = packed array[1..30] of char;

   order =
   packed record
      f: opcode;
      x: -lmax..+lmax;
      y: integer;
		instyp: types;
		line: integer
   end;
   orderarray = array[0..cmax] of order;

   objorder =
	packed record
		f: 0..omax;
		x: -lmax..lmax;
		y: integer;
		l: integer
	end;
   objorderarray = array[0..cmax] of objorder;

   tabrec =
	packed record
   		name: alfa;
   		link: index;
   		obj: myobject;
   		typ: types;
   		ref: index;
   		normal: boolean;
   		lev: 0..lmax;
   		taddr: integer;
			auxref: index
   	end;
	tabarray = array[0..tmax] of tabrec;

	atabrec =
  	packed record
      	inxtyp,eltyp:types;
      	inxref,elref,low,high,elsize,size:index;
   	end;
	atabarray = array[1..amax] of atabrec;

	btabrec =
  	packed record
      	last,lastpar,psize,vsize:index;
      	tabptr: 0..tmax
   end;
	btabarray = array[1..bmax] of btabrec;

	stabarray = packed array[0..smax] of char;
	realarray = array[1..rmax] of real;

   intabrec =
   packed record
      tp: types;
      lv:  0..lmax;
      rf: integer;
      vector: integer;
      off:  integer;
		tabref: integer
   end;
	intabarray = array[1..intermax] of intabrec;


(* unixtypes.i *)

(* Pascal-FC "universal" compiler system *)
(* implementation-dependent type declaration for Unix *)


   objcoderec =
	packed record
		fname:		fnametype;
		prgname:    alfa;
		gencode:    objorderarray;
		ngencode:   0..cmax;

		gentab:  tabarray;
		ngentab: 0..tmax;

		genatab:    atabarray;
		ngenatab: 0..amax;

		genbtab: btabarray;
		ngenbtab: 0..bmax;

		genstab: stabarray;
		ngenstab: 0..smax;
		genrconst: realarray;

      useridstart: 0..tmax;


	end;


   ptype=0..pmax;
	powerset = set of 0..bsmsb;

	qpointer = ^qnode;
	qnode =
	record
		proc: ptype;
		next: qpointer
	end;

	stackrec =
			record
				case tp: types of
					ints:		(i: integer);
					bitsets:	(bs: powerset);
					reals:		(r: real)
				end;


(* This type is declared within the GCP Run Time System *)
    UnixTimeType = LongInt;


var
  FileStream: TFileStream;
  NazevSouboru2:string;
  objfile: array of objcoderec;
  f: File of byte;
  curpr:ptype;

	stantyps: typset;
	ch: char;


      ir: objorder;
      ps:
			(run,fin,divchk,inxchk,charchk,stkchk,
   		redchk,deadlock,channerror,guardchk,queuechk,
			procnchk,statchk,nexistchk, namechk,casechk,
			bndchk,instchk,inpchk,setchk,ovchk,seminitchk);

      lncnt,chrcnt:integer;

		foundcall: boolean;		(* used in select (code 64) *)

      s: array[1..stmax] of stackrec;
      ptab:array[ptype] of
      record
         t,b,pc,stackbase,stacksize:integer;
         display:array[1..lmax] of integer;
         suspend:integer;
         chans: integer;
		     repindex: integer;
		     onselect: boolean;
         active, termstate:boolean;
         curmon: integer;
         wakeup, wakestart: integer;
			clearresource:  boolean;

			varptr: 0..tmax

      end;
      npr, procmax:ptype;
      stepcount:integer;
      concflag: boolean;
      statcounter: 0..maxint;
		sysclock: 0..maxint;


        (* I declare them to be UnixTimeType (lognints) *)
		now, last: UnixTimeType;

      procqueue: record
                  proclist: array [1..pmax] of
                      record
                        proc: ptype;
                        link: ptype
                      end;
                  free: 0..pmax
                end;

      eventqueue: record
                     first: qpointer;
                     time: integer
                  end;


   function itob(i:integer):boolean;
      begin
         if i=tru
         then itob:=true
         else itob:=false
      end;


   function btoi(b:boolean):integer;
      begin
         if b
         then btoi:=tru
         else btoi:=fals
      end;

   procedure putversion(var tofile: text);

	begin
		write(tofile,'- Interpreter Version P5.3');


		write(tofile,' - ')

	end;  (* putversion *)

		procedure printname(name: alfa; var tofile: text);

		var
			index:  integer;
			endfound: boolean;

		begin
			index := 1;
			endfound := name[index] = ' ';
			while not endfound do
				begin
				write(tofile,name[index]);
				index := index + 1;
				if index <= alng then
					endfound := (name[index] = ' ')
				else
					endfound := true
				end
		end;  (* printname *)


		procedure nameobj(target: integer;
								 var tp: types; var tofile: text);

		var
			tptr, procptr, offset, prtarget: integer;
			rf: index;

			procedure unselector(var rf: index; var tp: types);

			(* output array subscripts or record fields *)


				procedure arraysub(var rf: index; var tp: types);

				var
					sub: integer;

				begin
					write(tofile,'[');
					with  objfile[0].genatab[rf] do
						begin
						sub := (offset div elsize) + low;
						offset := offset mod elsize;
						case inxtyp of
							ints,
							enums:		write(tofile,sub:1);
							chars:		write(tofile,'''',chr(sub),'''');
							bools:		write(tofile,itob(sub))
						end;
						write(tofile,']');
						tp := eltyp;
						rf := elref
						end
				end;  (* arraysub *)


				procedure recfield(var rf: index; var tp: types);

				var
					tptr: integer;


				begin
					write(tofile,'.');
					with objfile[0] do
						begin
						tptr := genbtab[rf].last;
						while gentab[tptr].taddr > offset do
							tptr := gentab[tptr].link;
						printname(gentab[tptr].name,tofile);
						rf := gentab[tptr].ref;
						tp := gentab[tptr].typ;
						offset := offset - gentab[tptr].taddr
						end  (* with *)
				end;  (* recfield *)




			begin
				repeat
					if tp = arrays then
						arraysub(rf,tp)
					else
						recfield(rf,tp)
				until not (tp in [arrays,records])
			end;  (* unselector *)


			procedure followlinks(target: integer; var tptr, offset: integer);

			var
				dx: integer;

			begin
				with objfile[0] do
					begin
					while gentab[tptr].obj <> variable do
						tptr := gentab[tptr].link;
					dx := gentab[tptr].taddr;
					while dx > target do
						begin
						tptr := gentab[tptr].link;
						if gentab[tptr].obj = variable then
							dx := gentab[tptr].taddr
						end  (* while *)
					end;  (* with *)
				offset := target - dx
			end;  (* followlkins *)

			procedure monitortyp(target: integer; var tptr, offset: integer);

			(* name monitor boundary queue, h-p queue
            or any variable declared in a monitor *)

			begin
				with objfile[0] do
					begin
					printname(gentab[tptr].name,tofile);
					if offset = 0 then
						if gentab[tptr].typ = monvars then
							write(tofile,' (monitor boundary queue)')
						else
							write(tofile,' (resource boundary queue)')
					else
						if offset = 1 then
							begin  (* h-p queue *)
							printname(gentab[tptr].name,tofile);
							write(tofile,' (monitor high-priority queue)')
							end
						else
							begin  (* declared variable *)
							write(tofile,'.');
							tptr := genbtab[gentab[tptr].ref].last;
							followlinks(target,tptr,offset);
							if gentab[tptr].typ = protq then
								printname(gentab[tptr-1].name,tofile)
							else
								printname(gentab[tptr].name,tofile);
							rf := gentab[tptr].ref;
							tp := gentab[tptr].typ
							end
					end  (* with *)
			end;  (* monitortyp *)

			procedure entryname(bref: integer);

			var
				tptr: integer;


			begin
				write(tofile,'.');
				target := ((target - ptab[1].stackbase) mod stkincr);
				with objfile[0] do
					begin
					tptr := genbtab[bref].last;
					followlinks(target,tptr,offset);
					printname(gentab[tptr].name,tofile);
					end  (* with *)
			end;  (* entryname *)



		begin  (* Nameobj *)
			if target > ptab[0].stacksize then
				begin
				procptr := ((target - ptab[1].b) div stkincr) + 1;
				prtarget := ptab[procptr].varptr
				end
			else
				prtarget := target;
			with objfile[0] do
				begin
				tptr := genbtab[2].last;
				followlinks(prtarget,tptr,offset);
				rf := gentab[tptr].ref;
				tp := gentab[tptr].typ;
				if tp in [monvars,protvars] then
					monitortyp(target,tptr,offset)
				else
					printname(gentab[tptr].name,tofile)
				end;  (* with *)
			if tp in [arrays,records] then
				unselector(rf,tp);
			if target > ptab[0].stacksize then
				begin
				entryname(rf);
				tp := entrys
				end
		end;  (* nameobJ *)


	procedure getcode;

	(* get code from objfile *)
  var
   size: integer;
   NazevSouboru2:string;

	begin

		{reset(objfile)}
    NazevSouboru2:=   ChangeFileExt(NazevSouboru, '.obj');
    FileStream:= TFileStream.Create(NazevSouboru2, fmOpenRead);
    SetLength(objfile, FileStream.Size);
    FileStream.ReadBuffer(objfile[0], Length(objfile));



	end;  (* getcode *)



 	procedure headermsg(tp: types; var tofile: text);


		begin
      	with ptab[curpr] do
				begin
         	write(tofile, 'Abnormal halt ');
				if active then
					begin
					if curpr = 0 then
						write(tofile,'in main program ')
					else
						begin
						write(tofile,'in process ');
						nameobj(varptr,tp,tofile)
						end;
					writeln(tofile,' with pc = ', pc:1)
					end
				else
					begin
					write(tofile,'on termination of process ');
					nameobj(varptr,tp,tofile);
					writeln(tofile)
					end
				end;
      	write(tofile,'Reason:   ');

      	case ps of
         	deadlock:
            	writeln(tofile,'deadlock');
         	divchk:
            	writeln(tofile,'division by 0');
         	inxchk:
            	writeln(tofile,'invalid index ');
				charchk:
					writeln(tofile,'illegal or uninitialised character');
         	stkchk:
            	writeln(tofile,'storage overflow');
         	redchk:
            	writeln(tofile,'reading past end of file');
				channerror:
					writeln(tofile,'channel error');
				guardchk:
					writeln(tofile,'closed guards');
         	procnchk:
					writeln(tofile,'more than ',pmax:1,' processes');
         	statchk:
					writeln
						(tofile,'statement limit of ',statmax:1,
						' reached (possible livelock)');
				nexistchk:
					writeln(tofile,
					'attempt to call entry of non-existent/terminated process');
				namechk:
         		writeln(tofile,
					'attempt to make entry on process without unique name');
				casechk:
					writeln(tofile,
						'label of ',s[ptab[curpr].t].i:1,' not found in case');
				bndchk:
					writeln(tofile,'ordinal value out of range');
				instchk:
					writeln(tofile,'multiple activation of a process');
				inpchk:
					writeln(tofile,'error in numeric input');
				setchk:
					writeln(tofile,'bitset value out of bounds');
				ovchk:
					writeln(tofile,'arithmetic overflow');
				seminitchk:
					writeln(tofile,'attempt to initialise semaphore from process')
				end;  (* case *)

         	writeln(tofile); writeln(tofile)
		end;  (* headermsg *)


		procedure printyp(tp: types; var tofile: text);

		begin
			case tp of
				semafors:	writeln(tofile,' (semaphore)');
				condvars:	writeln(tofile,' (condition)');
				monvars,
				protvars:		writeln(tofile);
				channels:	writeln(tofile,' (channel)');
				entrys:		writeln(tofile,' (entry)');
				procs:		;
				protq:		writeln(tofile,' (procedure guard)')
			end
		end;  (* printyp *)



		procedure oneproc(nproc: integer);


		(* give pmd report on one process *)

		var
			tp: types;
			loop, frameptr, chanptr: integer;

		begin

			writeln(pmdfile,'----------');

			with ptab[nproc] do
				begin
				if nproc = 0 then

					writeln(pmdfile,'Main program')

				else
					begin

					write(pmdfile,'Process ');
					nameobj(varptr,tp,pmdfile);
					writeln(pmdfile)

					end;

				writeln(pmdfile);
				write(pmdfile,'Status:  ');

				if active then
					begin

					writeln(pmdfile,'active');
					writeln(pmdfile,'pc = ',pc:1)

					end
				else
					if nproc = 0 then

						writeln(pmdfile,'awaiting process termination')

					else

						writeln(pmdfile,'terminated');

				if termstate or (suspend <> 0) then
					begin

					writeln(pmdfile);
					writeln(pmdfile,'Process suspended on:');
					writeln(pmdfile);

            	if suspend > 0 then
               	begin

						nameobj(suspend,tp,pmdfile);
						printyp(tp,pmdfile)

						end
					else
						begin
						frameptr := chans;
						for loop := 1 to abs(suspend) do
							begin
							chanptr := s[frameptr].i;
							if chanptr <> 0 then	(* 0 means timeout *)
								begin

								nameobj(chanptr,tp,pmdfile);
								printyp(tp,pmdfile)

								end
							else

								writeln(pmdfile,'timeout alternative');

							frameptr := frameptr + sfsize
							end
						end;
					if termstate then

						writeln(pmdfile,'terminate alternative')

					end
				end;  (* with *)

			writeln(pmdfile); writeln(pmdfile)

		end;  (* oneproc *)



		procedure globals;


		(* print global variables *)

		var
			h1: integer;
			noglobals: boolean;

		begin
			noglobals := true;

			writeln(pmdfile);
			writeln(pmdfile,'==========');
			writeln(pmdfile,'Global variables');
			writeln(pmdfile);

			with objfile[0] do
				begin
         	h1 := genbtab[2].last;
         	while gentab[h1].link <> 0 do
            	with gentab[h1] do
               	begin
               	if obj=variable   then
                  	if typ in (stantyps + [semafors,enums]) then
								begin
								noglobals := false;
                     	case typ of
                        	ints, semafors, enums:

                           	writeln(pmdfile,string(name),' = ',s[taddr].i);

									reals:

										writeln(pmdfile,string(name),' = ',s[taddr].r);

                        	bools:

                           	writeln(pmdfile,string(name),' = ',itob(s[taddr].i));

                        	chars:

                              writeln(pmdfile,string(name),' = ',chr(s[taddr].i mod 64));

                     	end   (* case *)
							end;  (* if *)
               		h1 := link
               	end  (* with gentab *)
					end;  (* with objfile^ *)
			if noglobals then

				writeln(pmdfile,'(None)')

		end;  (* globals *)





   procedure expmd;

   (* print post-mortem dump on execution-time error *)

	var
		h1: integer;
		tp: types;


   begin  (* Expmd *)
		rewrite(pmdfile);
		write(pmdfile,'Pascal-FC post-mortem report on ');
		printname(objfile[0].prgname,pmdfile);
		writeln(pmdfile);
		putversion(pmdfile);
		writeln(pmdfile);
		headermsg(tp,pmdfile);
		headermsg(tp,output);
		writeln;
		writeln('See pmdfile for post-mortem report');
      for h1 := 0 to procmax do

			oneproc(h1);
		if (curpr <> 0) or (ps <> stkchk) then
			globals

   end ; (* expmd *)



(* real-time clock management module *)

{ Get the real time. MicroSecond can be Null and is ignored then. }
{function  GetUnixTime (var MicroSecond: Integer): UnixTimeType;
  asmname '_p_GetUnixTime'; }

  function GetUnixTime(var MicroSecond: integer): UnixTimeType;
  begin
    MicroSecond := 1;
  end;


	procedure initclock;

    var
        microsecs : integer;

	begin
        microsecs := 0;
		sysclock := 0;
		last := GetUnixTime(microsecs)
	end;  (* initclock *)


	procedure checkclock;

    var
        microsecs : integer;

	begin
		now := GetUnixTime(microsecs);
		if now <> last then
			begin
			last := now;
			sysclock := sysclock + 1
			end
	end;  (* checkclock *)

	procedure doze(n: integer);

	begin
		while eventqueue.time > sysclock do
			checkclock
	end;  (* doze *)



	procedure runprog;

	(* execute program once *)

	label
      97,98;

   var
    h1,h2,h3,h4:integer;
    curpr:ptype;


	procedure getqueuenode(pnum: ptype; var ptr: qpointer);

	(* place pnum in a dynamic queue node *)

	begin
		new(ptr);
		with ptr^ do
			begin
			proc := pnum;
			next := nil
			end
	end;  (* getqueuenode *)



   procedure joineventq(waketime: integer);

   (* join queue of processes which have executed a "sleep" *)


   var
      thisnode, frontpointer, backpointer: qpointer;
      foundplace: boolean;

   begin
		with ptab[curpr] do
			begin
			wakeup := waketime;
			if wakestart = 0 then
				wakestart := pc
			end;
      stepcount := 0;
      getqueuenode(curpr,thisnode);
		with eventqueue do
			begin
			frontpointer := first;
			if frontpointer <> nil then
				begin
				backpointer := nil;
				foundplace := false;
				while not foundplace and (frontpointer <> nil) do
					if ptab[frontpointer^.proc].wakeup > waketime then
						foundplace := true
					else
						begin
						backpointer := frontpointer;
						frontpointer := backpointer^.next
						end;
					thisnode^.next := frontpointer;
					if backpointer <> nil then
						backpointer^.next := thisnode
				end;  (* if first <> nil *)
			if frontpointer = first then
					begin
					first := thisnode;
					time := waketime
					end
			end  (* with eventqueue *)
   end;  (* joineventq *)


	procedure leventqueue(pnum: ptype);

	(* process pnum is taken from event queue *)
	(* (a rendezvous has occurred before a timeout alternative expires) *)

	var
		frontpointer, backpointer: qpointer;
		found: boolean;

	begin
		with eventqueue do
			begin
			frontpointer := first;
			backpointer := nil;
			found := false;
			while not found and (frontpointer <> nil) do
				if frontpointer^.proc = pnum then
					found := true
				else
					begin
					backpointer := frontpointer;
					frontpointer := frontpointer^.next
					end;
			if found then
				begin
				if backpointer = nil then
					begin
					first := frontpointer^.next;
					if first <> nil then
						time := ptab[first^.proc].wakeup
					else
						time := 0
					end
				else
					backpointer^.next := frontpointer^.next;
				dispose(frontpointer)
				end  (* if found *)
			end  (* with eventqueue *)
		end;  (* leventqueue *)


      procedure alarmclock; forward;

      procedure chooseproc;

      (* modified to permit a terminate option on select - gld *)
      label 98;
      var d: integer;
          procindex: integer;
          foundproc, procwaiting: boolean;

      begin
         foundproc:=false;
         repeat
            procwaiting:=false;
            d:=procmax+1;

            procindex:=(curpr+trunc(random*procmax))mod(procmax+1);

            while not foundproc and (d>=0) do
               with ptab[procindex] do
                    begin
                  foundproc := active and (suspend=0) and
                     (wakeup=0) and not termstate;
                  if not foundproc then
                     begin
                     if active and not termstate then
                        procwaiting := true;
                     d:=d-1;procindex:=(procindex+1) mod (procmax+1)
                     end  (* if not foundproc *)
                  end;  (* with *)
            if not foundproc then
               if procwaiting then
                  if eventqueue.first <> nil then
							begin
							doze(eventqueue.time - sysclock);
                     alarmclock
							end
                  else
                     begin
                     ps:=deadlock;goto 98
                     end
               else
                  ptab[0].active := true
            else
               begin
               curpr := procindex;
               stepcount:=trunc(random*stepmax)
               end
         until foundproc or (ps <> run);
      98:;
      end;  (* chooseproc *)



		procedure clearchans(pnum, h: integer);

		(* clear all channels on which the process sleeps *)

		var
			loop, nchans, frameptr, chanptr: integer;

		begin
			with ptab[pnum] do
				begin
         	nchans := abs(suspend);
				frameptr := chans;
         	for loop := 1 to nchans do
            	begin
					chanptr := s[frameptr].i;
					if chanptr <> 0 then  (* timeout if 0 *)
						begin
            		s[chanptr].i := 0;
            		if chanptr = h then
							if onselect then
								begin
               			repindex := s[frameptr+5].i;
								onselect := false
								end
						end;
					frameptr := frameptr + sfsize
            	end;
				chans := 0;
         	suspend:=0;
         	termstate:=false;
				end  (* with *)
		end;  (* clearchans *)



   procedure wakenon(h: integer);

   (* awakens the process asleep on this channel *)
   (* also used to wake a process asleep on several entries
      in a select statement, where it cannot be in a queue *)

   var procn: integer;

   begin
      procn := s[h+2].i;
      with ptab[procn] do
         begin
			clearchans(procn,h);
			leventqueue(procn);
			wakeup := 0;
         pc:=s[h+1].i

         end  (* with ptab[procn] *)

   end;  (* wakenon *)


   procedure initqueue;

   (* initialise process queue *)

   var index: 1..pmax;

   begin  (* initqueue *)
      with procqueue do
         begin
         free:=1;
         for index :=1 to pmax-1 do
            proclist[index].link:=index+1;
         proclist[pmax].link:=0
         end  (* with *)
   end;  (* initqueue *)


   procedure getnode(var node: ptype);

   (* get a node from the free list for process queues *)
   (* the link is set to zero *)

   begin  (* getnode *)
      with procqueue do
         if free=0 then
            ps := queuechk
         else
            begin
            node := free;
            free := proclist[node].link;
            proclist[node].link := 0
            end
   end;  (* getnode *)


   procedure disposenode(node: ptype);

   (* return monitor queue node to free list *)

   begin  (* disposenode *)
      with procqueue do
         begin
         proclist[node].link := free;
         free := node
         end
   end;  (* disposenode *)


   procedure joinqueue(add: integer);

   (* join a process queue *)
   (* add is the stack address of the condvar or monvar *)

   var newnode, temp: ptype;

   begin  (* joinqueue *)
      ptab[curpr].suspend := add;
      stepcount := 0;
      getnode(newnode);
      procqueue.proclist[newnode].proc := curpr;
      if s[add].i < 1 then
         s[add].i := newnode
      else
         begin
         temp:= s[add].i;
         with procqueue do
            begin
            while proclist[temp].link <> 0 do
               temp := proclist[temp].link;
            proclist[temp].link := newnode
            end
         end
   end;  (* joinqueue *)


      procedure alarmclock;

      (* wake processes on event queue *)

      var
         now: integer;
         frontpointer, backpointer: qpointer;
         finished: boolean;

      begin
         now := eventqueue.time;
         finished := false;
         with eventqueue do
				begin
				frontpointer := first;
            while (frontpointer <> nil) and not finished do
               begin
					with ptab[frontpointer^.proc] do
						begin
						clearchans(frontpointer^.proc,0);
						wakeup := 0;
						pc := wakestart;
						wakestart := 0
						end;
					backpointer := frontpointer;
					frontpointer := frontpointer^.next;
					dispose(backpointer);
					if frontpointer <> nil then
						finished := ptab[frontpointer^.proc].wakeup <> now
               end;  (* while *)
				first := frontpointer;
				if frontpointer = nil then
					time := 0
				else
					time := ptab[frontpointer^.proc].wakeup
				end (* with eventqueue *)
      end;  (* alarmclock *)



   procedure procwake(add: integer);

   (* wakes the first process in a monitor queue *)
   (* add is the stack address of the condvar or monvar *)

   var pr, node: ptype;

   begin  (* procwake *)
      if s[add].i>0 then
         begin
         node := s[add].i;
         pr := procqueue.proclist[node].proc;
         s[add].i := procqueue.proclist[node].link;
         disposenode(node);

         ptab[pr].suspend := 0
         end
   end;  (* procwake *)


   procedure releasemon(curmon: integer);

   (* release mutual exclusion on a monitor *)


   begin

      if s[curmon+1].i > 0 then
         procwake(curmon+1)
      else
         if s[curmon].i > 0 then
               begin
               procwake(curmon);
               if s[curmon].i = 0 then
                  s[curmon].i := -1
               end
         else
            s[curmon].i := 0
   end;  (* releasemon *)


	procedure skipblanks;

	begin
		while  not eof and (ch = ' ') do
			{get(input)}
      read(ch);
	end;  (* skipblanks *)



	procedure readunsignedint(var inum: integer;  var numerror: boolean);

	var
		digit: integer;

	begin  (* Readunsignedint *)
      inum := 0;
      numerror := false;
      repeat
         if inum > (intmax div 10) then
            numerror := true
         else
            begin
            inum := inum*10;
            digit := ord({input^} ch) - ord('0');
            if digit > (intmax - inum) then
               numerror := true
            else
               inum := inum + digit
            end;
         {get(input)}
         read(ch);
      until not ({input^} ch in['0'..'9']);
      if numerror then
      	inum := 0
	end;  (* readunsignedint *)

 	procedure readbasedint(var inum: integer;  var numerror: boolean);

	(* on entry inum has been set by unsignedint *)

	var
		digit, base: integer;
		negative: boolean;

	begin
         {get(input);}
         read(ch);
         if (inum in [2, 8, 16]) then
				base := inum
			else
				begin
				base := 16;
				numerror := true
				end;
			inum := 0; negative := false;
         repeat
            if negative then
               numerror := true
            else
               begin
               if inum > (intmax div base) then
                  begin
						if inum <= (intmax div (base div 2)) then
                  	negative := true
						else
							numerror := true;
                  inum := inum mod (intmax div base + 1)
                  end;
               inum := inum*base;
               if {input^} ch in ['0'..'9'] then
                  digit := ord({input^} ch) - ord('0')
               else
						if {input^} ch in ['A'..'Z'] then
							digit := ord({input^} ch) - ord('A') + 10
						else
							if {input^} ch in ['a'..'z'] then
                        		digit := ord({input^} ch) - ord('a') + 10
							else
								numerror := true;
               if digit >= base then
                  numerror := true
               else
                  inum := inum + digit
               end;
           { get(input) }
           read(ch);
         until not ({input^} ch in ['0'..'9','A'..'Z','a'..'z']);
         if negative then
				if inum = 0 then
					numerror := true
				else
					inum := (-maxint + inum) - 1;
         if numerror then
            inum := 0
	end;  (* readbasedint *)


	procedure findstart(var sign: integer);

	(* find start of integer or real *)

	begin
		skipblanks;
		if eof then
			ps := redchk
		else
			begin
			sign := 1;
			if ch = '+' then
				{get(input)}
        read(ch)
			else
				if ch = '-' then
					begin
					{get(input); }
          read(ch);
					sign := -1
					end
			end
	end;  (* findstart *)




	procedure readint(var inum: integer);

	var
		sign: integer;
		numerror: boolean;

	begin  (* Readint *)
		findstart(sign);
		if not eof then
			begin
			if ch in ['0'..'9'] then
				begin
				readunsignedint(inum,numerror);
				inum := inum * sign;
				if ch = '#' then
					readbasedint(inum,numerror)
				end
			else
				numerror := true;
			if numerror then
				ps := inpchk
			end
	end;  (* readint *)



	procedure readscale(var e: integer; var numerror: boolean);

	var
		s, sign, digit: integer;
    ch:char;

	begin
		{get(input);}
    read(ch);
		sign := 1; s := 0;
		if {ch} ch = '+' then
			{get(input)}
      read(ch)
		else
			if {ch } ch= '-' then
				begin
				{get(input);}
        read(ch);
				sign := -1
				end;
		if not ({ch} ch in ['0'..'9']) then
			numerror := true
		else
			repeat
				if s > (intmax div 10) then
					numerror := true
				else
					begin
					s := 10*s;
					digit :=  ord(ch) - ord('0');
					if digit > (intmax - s) then
						numerror := true
					else
						s := s + digit
					end;
				{get(input)}
        read(ch);
			until not ({ch} ch in ['0'..'9']);
		if numerror then
			e := 0
		else
			e := s*sign + e
	end;  (* readscale *)


	procedure adjustscale(var rnum: real; k,e: integer; var numerror: boolean);

	var
		s: integer;
		d, t: real;

	begin
	 	if (k + e) > emax then
			numerror := true
		else
				begin
				while e < emin do
					begin
					rnum := rnum/10.0;
					e := e + 1
					end;
				s := abs(e); t := 1.0; d := 10.0;
				repeat
					while not odd(s) do
						begin
						s := s div 2; d := sqr(d)
						end;
					s := s - 1; t := d*t
				until s = 0;
				if e >= 0 then
					if rnum > (realmax/t) then
						numerror := true
					else
						rnum := rnum *t
				else
					rnum := rnum/t
				end
	end;  (* adjustscale *)


	procedure readreal(var rnum: real);

	var
		k,e, sign, digit: integer;
		numerror: boolean;

	begin
		numerror := false;
		findstart(sign);
		if not eof then
			if ch in ['0'..'9'] then
				begin
				while ch = '0' do
					{get(input);}
          read(ch);
				rnum := 0.0; k := 0; e := 0;
				while ch in ['0'..'9'] do
					begin
					if rnum > (realmax/10.0) then
						e := e + 1
					else
						begin
						k := k + 1;
						rnum := rnum * 10.0;
						digit := ord(ch) - ord('0');
						if digit <= (realmax - rnum) then
							rnum := rnum + digit
						end;
					{get(input) }
          read(ch);
					end;
				if ch = '.' then
					begin  (* fractional part *)
					{get(input);}
          read(ch);
					repeat
						if ch in ['0'..'9'] then
							begin
							if rnum <= (realmax/10.0) then
								begin
								e := e - 1;
								rnum := 10.0*rnum;
								digit := ord(ch) - ord('0');
								if digit <= (realmax - rnum) then
									rnum := rnum + digit
								end;
							{get(input)}
              read(ch);
							end
						else
							numerror := true
					until not (ch in ['0'..'9']);
					if ch in ['e','E'] then readscale(e,numerror);
					if e <> 0 then adjustscale(rnum,k,e,numerror)
					end  (* fractional part *)
				else
					if ch in ['e','E'] then
						begin
						readscale(e,numerror);
						if e <> 0 then adjustscale(rnum,k,e,numerror)
						end
					else
						if e <> 0 then
							numerror := true;
				rnum := rnum * sign
				end
			else
				numerror := true;
		if numerror then
			ps := inpchk
	end;  (* readreal *)



begin (* Runprog *)
	stantyps := [ints,reals,chars,bools];
      writeln;
      writeln('Program ',string(objfile[0].prgname),'...  execution begins ...');
      writeln; writeln;
      initqueue;
      s[1].i:=0;s[2].i:=0;s[3].i:=-1;s[4].i:=objfile[0].genbtab[1].last;
      with ptab[0] do
      begin
         stackbase := 0; b:=0;suspend:=0;display[1]:=0;
			pc:=objfile[0].gentab[s[4].i].taddr;
         active:=true; termstate:=false;stacksize:=stmax-pmax*stkincr;
         curmon := 0; wakeup := 0; wakestart := 0;
			onselect := false;
         t:=objfile[0].genbtab[2].vsize-1;
			if t > stacksize then
				begin
				ps := stkchk;
				goto 98
				end;
         for h1:=5 to t do s[h1].i:=0
      end;
      for curpr := 1 to pmax do with ptab[curpr] do
         begin
            active := false; termstate:=false;
            display[1] := 0; pc := 0; suspend := 0;  curmon := 0;
            wakeup := 0; wakestart := 0;
            stackbase := ptab[curpr-1]. stacksize+1;
				b := stackbase;
				stacksize := stackbase + stkincr-1;
            t:= b-1 ;
				onselect := false;
				clearresource := true
         end;
      npr :=0; procmax := 0; curpr:=0;
      stepcount:=0;
      ps := run; lncnt:=0;chrcnt:=0;
      concflag:=false;
      statcounter := 0;
		initclock;

      with eventqueue do
         begin
         first := nil;
         time := -1
         end;


      repeat
         if (ptab[0].active) and (ptab[0].suspend = 0) and (ptab[0].wakeup=0) then
            curpr := 0
         else
            if stepcount = 0  then
               chooseproc
            else
               stepcount := stepcount - 1;
         with ptab[curpr] do
            begin

            ir := objfile[0].gencode[pc];

            pc:=pc+1

            end;
         if concflag then curpr := npr;

         with ptab[curpr] do
         case ir.f of

            0:
               begin
                  (*load address*) t:=t+1;
                  if t > stacksize
                  then ps := stkchk
                  else s[t].i := display[ir.x]+ir.y
               end;

            1:
               begin
                  (*load value*) t:= t +1;
                  if t > stacksize
                  then ps := stkchk

                  else s[t] := s[display[ir.x]+ir.y]

               end;

            2:
               begin
                  (*load indirect*) t := t +1;
                  if t > stacksize
                  then ps := stkchk
                  else s[t] := s[s[display[ir.x]+ir.y].i]
               end;

            3:
               begin
                  (*update display*)
                  h1 := ir.y;h2:=ir.x;h3:=b;
                  repeat
                     display[h1] := h3;h1:=h1-1;h3:=s[h3+2] .i
                  until h1 = h2
               end;

            4:
               (*cobegin*)
					;

            5:
               (*coend*)
               begin

                  procmax := npr;
                  ptab[0].active := false;
						stepcount := 0
               end;

            6:
               begin
                  (*wait*)
                  h1 := s[t].i;t:= t - 1;

                  if s[h1].i>0
                  then s[h1].i := s[h1].i - 1

                  else
                     begin
                        suspend:= h1; stepcount := 0
                     end
               end;

            7:
               begin
                  (*signal*)
                  h1 := s[t].i;t:=t-1;h2:=pmax+1;
						h3:=trunc(random*h2);
                  while (h2>=0) and (ptab[h3].suspend<> h1) do
                     begin
                        h3:=(h3+1) mod (pmax+1); h2:= h2-1
                     end;

                  if h2<0
                  then s[h1].i := s[h1].i + 1
                  else ptab[h3].suspend := 0

               end;

            8:
               case ir.y of
						0:
							s[t].i := abs(s[t].i);
						1:
							s[t].r := abs(s[t].r);
						2:	  (* integer sqr *)
							if (intmax div abs(s[t].i)) < abs(s[t].i) then
								ps := ovchk
							else
								s[t].i := sqr(s[t].i);
						3:	  (* real sqr *)
							if (realmax/abs(s[t].r)) < abs(s[t].r) then
								ps := ovchk
							else
								s[t].r := sqr(s[t].r);
						4:
							s[t].i := btoi(odd(s[t].i));
						5:	if not(s[t].i in [charl..charh]) then
								ps := charchk;
						6:	;
						7:	(* succ *)
							s[t].i := s[t].i + 1;
						8: (* pred *)
							s[t].i := s[t].i - 1;
						9:	  (* round *)
							if abs(s[t].r) >= (intmax + 0.5) then
								ps := ovchk
							else
								s[t].i := round(s[t].r);
						10:	(* trunc *)
							if abs(s[t].r) >= (intmax + 1.0) then
								ps := ovchk
							else
								s[t].i := trunc(s[t].r);
						11:
							s[t].r := sin(s[t].r);
						12:
							s[t].r := cos(s[t].r);
						13:
							s[t].r := exp(s[t].r);
						14:  (* ln *)
							if s[t].r <= 0.0 then
								ps := ovchk
							else
								s[t].r := ln(s[t].r);
						15:  (* sqrt *)
							if s[t].r < 0.0 then
								ps := ovchk
							else
								s[t].r := sqrt(s[t].r);
						16:
							s[t].r := arctan(s[t].r);

                  17:
                     begin
                        t:= t +1;
                        if t > stacksize
                        then ps := stkchk
                        else s[t].i := btoi(eof(input))
                     end;

                  18:
                     begin
                        t := t + 1;
                        if t > stacksize
                        then ps := stkchk
                        else s[t].i := btoi(eoln(input))
                     end;
                  19:
                     begin
                     h1 := abs(s[t].i) + 1;
                     s[t].i := trunc(random*h1)
                     end;
                  20:	(* empty *)
                     begin
                     h1 := s[t].i;
                     if s[h1].i=0 then
                        s[t].i := 1
                     else
                        s[t].i := 0
                     end;  (* f21 *)
						21:	(* bits *)
							begin
							h1 := s[t].i;
							s[t].bs := [];
							h3 := 0;
							if h1 < 0 then
								if bsmsb < intmsb then
									begin
									ps := setchk;
									h1 := 0
									end
								else
									begin
									s[t].bs := [bsmsb];
									h1 := (h1 + 1) + maxint;
									h3 := 1
									end;
							for h2 := 0 to bsmsb-h3 do
								begin
								if (h1 mod 2) = 1 then
									s[t].bs := s[t].bs + [h2];
								h1 := h1 div 2
								end;
							if h1 <> 0 then
								ps := setchk
							end;  (* f21 *)

						24:	(* int - bitset to integer *)
								begin
								h1 := 0;
								if bsmsb = intmsb then
									if intmsb in s[t].bs then
										h1 := 1;
								h2 := 0;  (* running total *)
								h3 := 1;  (* place value *)
								for h4 := 0 to bsmsb-h1 do
									begin
									if h4 in s[t].bs then
										h2 := h2 + h3;
									h3 := h3*2
									end;
								if h1 <> 0 then
									s[t].i := (h2-maxint) - 1
								else
									s[t].i := h2
								end;

						25:  (* clock *)
							begin
							t := t + 1;
							if t > stacksize then
								ps := stkchk
							else
								s[t].i := sysclock
							end;  (* f25 *)

               end;

       9:
                s[t].i := s[t].i + ir.y;

            10:
               pc := ir.y;

            (*jump*)
            11:
               begin
                  (*conditional jump*)
                  if s[t].i = fals
                  then pc := ir.y; t:= t-1;
               end;

				12:	(* case1 *)
					if s[t].i = s[t-1].i then
						begin
						t := t - 2;
						pc := ir.y
						end
					else
						t := t - 1;

				13:	(* case 2 *)
					ps := casechk;

            14:
               begin
                  (*for1up*) h1 := s[t-1].i;
                  if h1 <= s[t].i
                  then s[s[t-2].i].i := h1
                  else
                     begin
                        t := t-3; pc := ir.y
                     end
               end;

            15:
               begin
                  (*for2up*) h2 := s[t-2].i; h1 := s[h2].i +1;
                  if h1 <= s[t].i
                  then
                     begin
                        s[h2].i := h1; pc := ir.y
                     end
                  else t := t-3;
               end;

            18:
               begin
               if ir.x = 1 then
                  begin  (* process *)
                  if npr = pmax then
                     begin
                     ps := procnchk;
                     goto 98
                     end
                  else
                     begin
                     npr := npr + 1;
                     concflag:=true;
                     curpr := npr;
                     end
                  end;
                  h1 := objfile[0].genbtab[objfile[0].gentab[ir.y].ref].vsize;
                  with ptab[curpr] do
                     begin
                     if t+h1 > stacksize  then
                        ps := stkchk
                     else
                        begin
                        t := t+5; s[t-1].i := h1-1; s[t].i := ir.y
                        end
                     end  (* with *)
               end;

            19:
               begin
                  h1 := t-ir.y;
                  h2 := s[h1+4].i; (*h2 points to tab*)
                  h3 := objfile[0].gentab[h2].lev; display[h3+1] := h1;
                  h4 := s[h1+3].i+h1;
                  s[h1+1].i := pc; s[h1+2].i := display[h3];
                  if ir.x=1 then
                     begin  (* process *)
                     active := true;
                     s[h1+3].i := ptab[0].b ;
                     concflag := false
                     end
                  else
                     s[h1+3].i := b;
                  for h3 := t+1 to h4 do s[h3].i := 0;
                  b := h1; t := h4; pc := objfile[0].gentab[h2].taddr
               end;

            21:
					with objfile[0] do
               	begin
                  (*index*) h1 := ir.y; (*h1 points to genatab*)
                  h2 := genatab[h1].low; h3 := s[t].i;
                  if h3 < h2
                  then ps := inxchk
                  else
                  if h3 > genatab[h1].high
                  then ps := inxchk
                  else
                     begin
                        t := t-1; s[t].i := s[t].i + (h3-h2)*genatab[h1].elsize
                     end
               end;

            22:
               begin
                  (*load block*) h1 := s[t].i; t := t-1;
                  h2 := ir.y+t;
                  if h2 > stacksize
                  then ps := stkchk
                  else
                     while t < h2 do
                        begin
                           t := t+1; s[t] := s[h1]; h1 := h1+1
                        end
               end;

            23:
               begin
                  (*copy block*) h1 := s[t-1].i;
                  h2 := s[t].i; h3 := h1+ir.y;
                  while h1 < h3 do
                     begin
                        s[h1] := s[h2]; h1 := h1+1;h2 := h2+1
                     end;
                  t := t-2
               end;

            24:
               begin
                  (*literal*) t := t+1;
                  if t > stacksize
                  then ps := stkchk
                  else s[t].i := ir.y
               end;

				25:
					begin
					t := t + 1;
					if t > stacksize then
						ps := stkchk
					else
						s[t].r := objfile[0].genrconst[ir.y]
					end;

				26:
					begin  (* float *)
					h1 := t - ir.y;
					s[h1].r := s[h1].i
					end;





            27:
               begin
                  (*read*)
                  if eof(input)
                  then ps := redchk
                  else
                     case ir.y of
                        1:    (* integer *)
                           readint(s[s[t].i].i);

                        3:    (* char *)
									if eof then
										ps := redchk
									else
                           	begin
                              read(ch);s[s[t].i].i := ord(ch)
                           	end;
								4:  (* real *)
									readreal(s[s[t].i].r)
                     end;
                  t := t-1
               end;

            28:
               begin
                  (*write string*)
						if ir.x = 1 then
							begin
							h3 := s[t].i;
							t := t - 1
							end
						else
							h3 := 0;
                  h1 := s[t].i; h2 := ir.y;t := t-1;
                  chrcnt := chrcnt+h1+h3;
						while h3 > h1 do
							begin
							write(' ');
							h3 := h3 - 1
							end;
                  repeat
                     write(objfile[0].genstab[h2]);h1 := h1-1; h2 := h2+1
                  until h1=0
               end;

            29:
               begin
               case ir.y of
                  1:    (* ints *)
                     write(s[t].i);
                  2:  (* bools *)
                     write(itob(s[t].i));
                  3:    (* chars *)
                     if (s[t].i < charl) or (s[t].i > charh) then
                        ps := charchk
                     else
								write(chr(s[t].i));
						4:  (* reals *)
							write(s[t].r);
						5:  (* bitsets *)
								for h1 := bsmsb downto 0 do
									if h1 in s[t].bs then
										write('1')
									else
										write('0')
               end;   (* case *)
               t := t-1
               end;   (* s9 *)

				30:
					begin  (* write formatted *)
					h3 := s[t].i;  (* field width *)
					t := t - 1;
						case ir.y of
							1:
								write(s[t].i:h3);  (* ints *)
							2:
								write(itob(s[t].i):h3);  (* bools *)
							3:
								if (s[t].i < charl) or (s[t].i > charh) then
									ps := charchk
							else
								write(chr(s[t].i):h3);
							4:	write(s[t].r:h3);
							5:
								begin
								while h3 > (bsmsb + 1) do
									begin
									write(' ');
									h3 := h3 - 1
									end;
								for h1 := bsmsb downto 0 do
									if h1 in s[t].bs then
										write('1')
									else
										write('0')
								end
						end;  (* case *)
					t := t - 1
					end;  (* 30 *)

            31:
               ps := fin;

            32:
               begin
                  t := b -1; pc:= s[b+1].i;
                  if pc <> 0
                  then b := s[b+3].i
                  else
                     begin
                        npr := npr -1; active := false;
                        stepcount := 0; ptab[0].active := (npr = 0);

                     end
               end;

            33:
               begin
                  (* exit function *)
                  t:= b; pc := s[b+1].i; b:= s[b+3].i;
               end;

            34:

               s[t] := s[s[t].i];


            35:
               s[t].i := btoi(not(itob(s[t].i)));

            36:
               s[t].i := -s[t].i;

				37:
					begin		(* formatted reals output *)
					h3 := s[t-1].i;
					h4 := s[t].i;
					write(s[t-2].r:h3:h4);
					t := t - 3
					end;

            38:

               begin
                  (*store*) s[s[t-1].i] := s[t]; t:= t-2;

               end;

				39:
					begin
					t := t - 1;
					s[t].i := btoi(s[t].r = s[t+1].r)
					end;

				40:
					begin
					t := t - 1;
					s[t].i := btoi(s[t].r <> s[t+1].r)
					end;

				41:
					begin
					t := t - 1;
					s[t].i := btoi(s[t].r < s[t+1].r)
					end;

				42:
					begin
					t := t - 1;
					s[t].i := btoi(s[t].r <= s[t+1].r)
					end;

				43:
					begin
					t := t - 1;
					s[t].i := btoi(s[t].r > s[t+1].r)
					end;

				44:
					begin
					t := t - 1;
					s[t].i := btoi(s[t].r >= s[t+1].r)
					end;


            45:
               begin
                  t:= t-1; s[t].i := btoi(s[t].i = s[t+1].i);
               end;

            46:
               begin
                  t := t -1; s[t].i := btoi(s[t].i <> s[t+1].i);
               end;

            47:
               begin
                  t := t -1; s[t].i := btoi(s[t].i < s[t+1].i);
               end;

            48:
               begin
                  t := t -1; s[t].i := btoi(s[t].i <= s[t+1].i);
               end;

            49:
               begin
                  t := t -1; s[t].i := btoi(s[t].i > s[t+1].i);
               end;

            50:
               begin
                  t := t -1; s[t].i := btoi(s[t].i >= s[t+1].i);
               end;

            51:
               begin
                  t := t -1; s[t].i := btoi(itob(s[t].i) or itob(s[t+1].i));
               end;

            52:
               begin
               t := t -1;
					if ((s[t].i > 0) and (s[t+1].i > 0)) or
						((s[t].i < 0) and (s[t+1].i < 0)) then
						if (maxint - abs(s[t].i)) < abs(s[t+1].i) then
							ps := ovchk;
					if ps <> ovchk then
						s[t].i := s[t].i + s[t+1].i;
               end;

            53:
               begin
               t := t -1;
					if ((s[t].i < 0) and (s[t+1].i > 0)) or
						((s[t].i > 0) and (s[t+1].i < 0)) then
						if  (maxint - abs(s[t].i)) < abs(s[t+1].i) then
							ps := ovchk;
					if ps <> ovchk then
						s[t].i := s[t].i - s[t+1].i;
               end;

				54:
					begin
					t := t - 1;
					if ((s[t].r > 0.0) and (s[t+1].r > 0.0)) or
						((s[t].r < 0.0) and (s[t+1].r < 0.0)) then
							if (realmax - abs(s[t].r)) < abs(s[t+1].r) then
								ps := ovchk;
					if ps <>  ovchk then
						s[t].r := s[t].r + s[t+1].r
					end;

				55:
					begin
					t := t - 1;
					if ((s[t].r > 0.0) and (s[t+1].r < 0.0)) or
								  ((s[t].r < 0.0) and (s[t+1].r > 0.0)) then
									  if (realmax - abs(s[t].r)) < abs(s[t+1].r) then
								ps := ovchk;
					if ps <>  ovchk then
						s[t].r := s[t].r - s[t+1].r
					end;

            56:
               begin
                  t := t -1; s[t].i := btoi(itob(s[t].i) and itob(s[t+1].i));
               end;

            57:
               begin
               t := t -1;
					if s[t].i <> 0 then
						if (maxint div abs(s[t].i)) < abs(s[t+1].i) then
							ps := ovchk;
					if ps <> ovchk then
						s[t].i := s[t].i * s[t+1].i;
               end;

            58:
               begin
                  t := t -1;
                  if s[t+1].i = 0
                  then ps := divchk
                  else
                     s[t].i := s[t].i div s[t+1].i;
               end;

            59:
               begin
                  t := t -1;
                  if s[t+1].i = 0
                  then ps := divchk
                  else
                     s[t].i := s[t].i mod s[t+1].i;
               end;

				60:
					begin
					t := t - 1;
					if (abs(s[t].r) > 1.0) and (abs(s[t+1].r) > 1.0) then
						if (realmax/abs(s[t].r)) < abs(s[t+1].r) then
							ps := ovchk;
					if ps <> ovchk then
						s[t].r := s[t].r * s[t+1].r
					end;

				61:
					begin
					t := t - 1;
					if s[t+1].r < minreal then
						ps := divchk
					else
						s[t].r := s[t].r / s[t+1].r
					end;


            62:
               if eof(input)
               then ps := redchk
               else readln;

            63:
               begin
                  writeln; chrcnt := 0
               end;

            64:
               begin
               h1 := t;
               h2 := 0;
               while s[h1].i <> -1 do
                  begin
                  h1 := h1 - sfsize;
                  h2 := h2 + 1
                  end;  (* h2 is now the number of open guards *)
               if h2=0 then
                  begin
                  if ir.y=0 then
                     ps:=guardchk  (* closed guards and no else/terminate *)
                  else
                     if ir.y=1 then
                        termstate:=true
                  end
               else
                  begin  (* channels/entries to check *)
                  if ir.x=0 then
                     h3 := trunc(random*h2)  (* arbitrary choice *)
                  else
                     h3:=h2-1;  (* priority select *)
                  h4 := t-(sfsize-1)-(h3*sfsize);  (* h4 points to bottom of "frame" *)
                  h1 := 1;
						foundcall := false;
                  while not foundcall and (h1 <=  h2) do
                     begin
							if s[h4].i = 0 then
								begin	(* timeout alternative *)
								if s[h4+3].i < 0 then
									s[h4+3].i := sysclock
								else
									s[h4+3].i := s[h4+3].i + sysclock;
								if (wakeup = 0) or (s[h4+3].i < wakeup) then
									begin
									wakeup := s[h4+3].i;
									wakestart := s[h4+4].i
									end;
                     	h3 := (h3+1) mod h2;
                     	h4 := t - (sfsize-1) - (h3*sfsize);
                     	h1 := h1 + 1
								end
							else
								if s[s[h4].i].i <> 0 then
									foundcall := true
								else
									begin
                     		h3 := (h3+1) mod h2;
                     		h4 := t - (sfsize-1) - (h3*sfsize);
                     		h1 := h1 + 1
									end
                     end;  (* while not foundcall ... *)
                  if not foundcall then  (* no channel/entry has a call *)
                     begin
                     if ir.y <> 2 then  (* ie, if no else part *)
                        begin  (* sleep on all channels *)
                        if ir.y=1 then termstate:=true;
                        h1 := t - (sfsize-1) - ((h2-1)*sfsize);
								chans := h1;
                        for h3 := 1 to h2 do
                           begin
                           h4 := s[h1].i;  (* h4 points to channel/entry *)
									if h4 <> 0 then	(* 0 means timeout *)
										begin
                           	if s[h1+2].i=2 then
                              	s[h4].i := -s[h1+1].i (* query sleep *)
                           	else
                              	if s[h1+2].i=0 then
                                 	s[h4].i := h1+1
                              	else
                                 	if s[h1+2].i = 1 then
                                    	s[h4] := s[h1+1]  (* shriek sleep *)
                                 	else
                                    	s[h4].i := -1;  (* entry sleep *)
                           	s[h4+1]:= s[h1+4];  (* wake address *)
                           	s[h4+2].i:=curpr
										end; (* if h4 <> 0 *)
                           h1:=h1+sfsize
                           end;  (* for loop *)
                        stepcount := 0;
                        suspend:=-h2;
								onselect := true;
								if wakeup <> 0 then
									joineventq(wakeup)
                        end (* sleep on open-guard channels/entries *)
                     end (* no call *)
                  else
                     begin  (* someone is waiting *)
							wakeup := 0;
							wakestart := 0;
                     h1 := s[h4].i;  (* h1 points to channel/entry *)
                     if s[h4+2].i in [0..2] then
                        begin  (* channel rendezvous *)
                        if ((s[h1].i<0) and (s[h4+2].i=2))
                         or ((s[h1].i>0) and (s[h4+2].i<2)) then
                           ps := channerror
                        else
                           begin  (* rendezvous *)
                           s[h1].i := abs(s[h1].i);
                           if s[h4+2].i=0 then
                            s[s[h1].i]:=s[h4+1]
                           else
                            begin  (* block copy *)
                            h3:=0;
                            while h3<s[h4+3].i do
                              begin
                              if s[h4+2].i=1 then
                                 s[s[h1].i+h3]:=s[s[h4+1].i+h3]
                              else
                                 s[s[h4+1].i+h3]:=s[s[h1].i+h3];
                              h3:=h3+1
                              end  (* while *)
                            end;  (* block copy *)
                           pc:=s[h4+4].i;
                           repindex  := s[h4+5].i;  (* recover repindex *)
                           wakenon(h1)  (* wake the other process *)
                           end  (* rendezvous *)
                        end  (* channel rendezvous *)
                     else
                        pc := s[h4+4].i  (* entry *)
                     end  (* someone was waiting *)
                  end;  (* calls to check *)
               t := t-1 - (h2*sfsize)
               end;  (* case 64 *)

            65:      (* channel write - gld *)
               begin
               h1 := s[t-1].i;   (* h1 now points to channel *)
               h2 := s[h1].i;   (* h2 now has value in channel[1] *)
               h3 := s[t].i;   (* base address of source (for ir.x=1) *)
               if h2>0 then
                  ps:=channerror  (* another writer on this channel *)
               else
                  if h2 = 0 then
                     begin  (* first *)
                     if ir.x=0 then s[h1].i:=t else s[h1].i := h3;
                     s[h1+1].i:=pc;
                     s[h1+2].i:=curpr;
                     chans := t-1;
                     suspend := -1;
                     stepcount := 0
                     end  (* first *)
                  else
                     begin  (* second *)
                     h2:=abs(h2);  (* readers leave negated address *)
                     if ir.x=0 then
                        s[h2]:=s[t]
                     else
                        begin
                        h4:=0;  (* loop control for block copy *)
                        while h4 < ir.y do
                           begin
                           s[h2 + h4] := s[h3 + h4];
                           h4 := h4 + 1
                           end  (* while *)
                        end;  (* ir.x was 1 *)
                     wakenon(h1)
                     end;  (* second *)
               t := t - 2
               end;  (* case 65 *)

            66:      (*  channel read - gld *)
               begin
               h1 := s[t-1].i;
               h2 := s[h1].i;
               h3 := s[t].i;
               if h2<0 then
                  ps:=channerror
               else
                  if h2 = 0 then
                     begin  (* first *)
                     s[h1].i := -h3;
                     s[h1+1].i:=pc;
                     s[h1+2].i:=curpr;
                     chans := t-1;
                     suspend := -1;
                     stepcount := 0
                     end  (* first *)
                  else
                     begin  (* second *)
                     h2:=abs(h2);
                     h4 := 0;
                     while h4 < ir.y do
                        begin
                        s[h3 + h4] := s[h2 + h4];
                        h4 := h4 + 1
                        end;
                     wakenon(h1)
                     end;
               t := t - 2
               end;  (* case 66 *)
         67:
               begin (* delay *)
               h1 := s[t].i;
               t := t - 1;
               joinqueue(h1);
					if curmon <> 0 then
               	releasemon(curmon)
               end;  (* case 67 *)


         68:
               begin  (* resume *)
               h1 := s[t].i;
               t := t - 1;
               if s[h1].i > 0  then
                  begin
                  procwake(h1);
						if curmon <> 0 then
                  	joinqueue(curmon+1)
                  end
               end;  (* case 68 *)

         69:
               begin  (* enter monitor *)
               h1 := s[t].i;  (* address of new monitor variable *)
               s[t].i := curmon;  (* save old monitor variable *)
               curmon := h1;
               if s[curmon].i = 0 then

                  s[curmon].i := -1

               else
                  joinqueue(curmon)
               end;  (* case 69 *)
         70:
               begin  (* exit monitor *)
               releasemon(curmon);
               curmon := s[t].i;
               t := t - 1
               end;  (* case 70 *)
         71:
               begin  (* execute monitor body code *)
               t := t +1;
               s[t].i := pc;
               pc := ir.y
               end;  (* case 70 *)
         72:
               begin  (* return from monitor body code *)
               pc := s[t].i;
               t := t - 1
               end;  (* case 72 *)

			74:	(* check lower bound *)
					if s[t].i < ir.y then
						ps := bndchk;

			75:	(* check upper bound *)
					if s[t].i > ir.y then
						ps := bndchk;

			78:
					;  (* no operation *)

			96:  (* pref *)
				;

         97:
               begin  (* sleep *)
               h1 := s[t].i;
               t := t - 1;
               if h1 <= 0 then
                  stepcount := 0
               else
                  joineventq(h1 + sysclock)
               end;  (* case 97 *)

         98:
               begin  (* set process var on process start-up *)
					h1 := s[t].i;
					varptr := h1;
               if s[h1].i = 0 then
                  s[h1].i := curpr
               else
                  ps := instchk;
               t := t - 1
               end;

         99:
               begin  (* ecall *)
                  h1 := t-ir.y;
                  t := h1 - 2;
                  h2 := s[s[h1-1].i].i;  (* h2 has process number *)
                  if h2 > 0  then
							if not ptab[h2].active then
								ps := nexistchk
							else
                     	begin
                     	h3 := ptab[h2].stackbase+s[h1].i;  (* h3 points to entry *)
                     	if s[h3].i <= 0 then
                        	begin  (* empty queue on entry *)
                        	if s[h3].i < 0 then
										begin  (* other process has arrived *)
										for h4 := 1 to ir.y do
											s[h3+h4+(entrysize-1)] := s[h1+h4];
                           	wakenon(h3)
										end;
                        	s[h3+1].i := pc;
                        	s[h3+2].i := curpr
                        	end;
                     	joinqueue(h3);
								s[t+1].i := h3;
                     	chans := t+1;
                     	suspend := - 1
                     	end
                  else
                     if h2 = 0 then
                        ps := nexistchk
                     else
                        ps := namechk
               end;

         100:
               begin    (* acpt1 *)
                  h1 := s[t].i;    (* h1 points to entry *)
                  t := t - 1;
                  if s[h1].i = 0 then
                     begin  (* no calls - sleep *)
                     s[h1].i := - 1;
                     s[h1+1].i := pc;
                     s[h1+2].i := curpr;
                     suspend := - 1;
                     chans := t+1;
                     stepcount := 0
                     end
						else
							begin  (* another process has arrived *)
							h2 := s[h1+2].i;  (* hs has proc number *)
                     h3 := ptab[h2].t + 3;  (* h3 points to first parameter *)
                     for h4 := 0 to ir.y - 1 do

                        s[h1+h4 + entrysize] := s[h3+h4]

							end
               end;

         101:

               begin  (* acpt2 *)
                  h1 := s[t].i; (* h1 points to entry *)
                  t := t - 1;
                  procwake(h1);

                  if s[h1].i <> 0 then
                     begin  (* queue non-empty *)
                     h2 := procqueue.proclist[s[h1].i].proc;  (* h2 has proc id *)
                     s[h1+1].i := ptab[h2].pc;
                     s[h1+2].i := h2
                     end
               end;

         102:	(* rep1c *)
               s[display[ir.x]+ir.y].i := repindex;

			103:	(* rep2c *)
					begin  (* replicate tail code *)
					h1 := s[t].i;
					t := t - 1;
					s[h1].i := s[h1].i + 1;
					pc := ir.y
					end;

			104:  (* powr2 *)

					begin
					h1 := s[t].i;
					if not (h1 in [0..bsmsb]) then
						ps := setchk
					else
						s[t].bs := [h1]
					end;  (* 104 *)

			105:	(* btest *)
					begin
					t := t - 1;
					h1 := s[t].i;
					if not (h1 in [0..bsmsb]) then
						ps := setchk
					else
						s[t].i := btoi(h1 in s[t+1].bs)
					end;  (* 105 *)

			107:  (* write based *)
					begin
					h3 := s[t].i;
					h1 := s[t-1].i;
					t := t - 2;
					if h3 = 8 then


						write(h1{:11:8})


					else


						write(h1{:8:16})


					end;  (* 107 *)


			112:
					begin
					t := t - 1;
					s[t].i := btoi(s[t].bs = s[t+1].bs)
					end;  (* 112 *)

			113:
					begin
					t := t - 1;
					s[t].i := btoi(s[t].bs <> s[t+1].bs)
					end;  (* 113 *)

			114:
					begin
					t := t - 1;
				   { btoi(s[t].bs < s[t+1].bs)}
            s[t].i := btoi((s[t].bs <= s[t + 1].bs) and
              (s[t].bs <> s[t + 1].bs));
					end;  (* 114 *)

			115:

					begin
					t := t - 1;
					s[t].i := btoi(s[t].bs <= s[t+1].bs)
					end;  (* 115 *)


			116:
					begin
					t := t - 1;
					{s[t].i :=
             btoi(s[t].bs > s[t+1].bs)   }
          s[t].i := btoi((s[t].bs >= s[t + 1].bs) and
              (s[t].bs <> s[t + 1].bs));
					end;  (* 116 *)

			117:
					begin
					t := t - 1;
					s[t].i := btoi(s[t].bs >= s[t+1].bs)
					end;  (* 117 *)

			118:
					begin
					t := t - 1;
					s[t].bs := s[t].bs + s[t+1].bs
					end;  (* 118 *)

			119:
					begin
					t := t - 1;
					s[t].bs := s[t].bs - s[t+1].bs
					end;  (* 119 *)

			120:
					begin
					t := t - 1;
					s[t].bs := s[t].bs * s[t+1].bs
					end;  (* 120 *)
			121:	(* sinit *)
					if curpr <> 0 then
						ps := seminitchk
					else
						begin
						s[s[t-1].i] := s[t];
						t := t - 2
						end;

			129:
					begin (* prtjmp *)
					if s[curmon+2].i = 0 then
						pc := ir.y
					end;
			130:
					begin (* prtsel *)
               h1 := t;
               h2 := 0;
					foundcall := false;
               while s[h1].i <> -1 do
                  begin
                  h1 := h1 - 1;
                  h2 := h2 + 1
                  end;  (* h2 is now the number of open guards *)
					if h2 <> 0 then
						begin  (* barriers to check *)
                  h3 := trunc(random*h2);  (* arbitrary choice *)
						h4 := 0;  (* count of barriers tested *)
                  while not foundcall and (h4 <  h2) do
                    	begin
							if s[s[h1 + h3 + 1].i].i <> 0 then
								foundcall := true
							else
								begin
                     	h3 := (h3+1) mod h2;
                     	h4 := h4 + 1
								end
							end;
						end;  (* barriers to check *)
					if not foundcall then
						releasemon(curmon)
					else
						begin
						h3 := s[h1 + h3 +1].i;
						procwake(h3)
						end;
					t := h1 - 1;
					s[curmon+2].i := 0;
					pc := s[t].i;
					t := t - 1
					end;
			131:
					begin (* prtslp *)
               h1 := s[t].i;
               t := t - 1;
               joinqueue(h1)
					end;
			132:
					begin (* prtex *)
					if ir.x = 0 then
						clearresource := true
					else
						clearresource := false;
					curmon := s[t].i;
					t := t - 1
					end;
			133:	(* prtcnd *)
					if clearresource then
						begin
						s[curmon+2].i := 1;
						t := t + 1;
						s[t].i := pc;
						t := t + 1;
						s[t].i := -1;
						pc := ir.y
						end

         end  (*case*);

			checkclock;

         if eventqueue.first <> nil then
            if eventqueue.time <= sysclock then
               alarmclock;
         statcounter := statcounter + 1;;
         if statcounter >= statmax then
            ps := statchk
      until ps <> run;

      98: writeln;
      if ps <> fin then

         expmd

      else
         begin
         writeln;
         writeln('Program terminated normally')
         end;
      97: writeln
	end;  (* runprog *)



begin
   assignFile(pmdfile,ChangeFileExt(NazevSouboru, '.prn'));
   randomize;
    (*
       Randomize is used to implement the
       native function: random, provided
       by GNU Pascal to implement randomness
     *)
    NazevSouboru2:=ChangeFileExt(NazevSouboru, '.out');
    assignFile(output,NazevSouboru2 );
    rewrite(output);
    putversion(output);
    rewrite(pmdfile);

	getcode;

	repeat

		runprog;
		writeln;
		writeln('Type r and RETURN to rerun');


	until not (ch in ['r','R']);
  CloseFile(output);
  CloseFile(pmdfile);


end;

procedure TForm1.Replace1Click(Sender: TObject);
begin
  FindDialog1.CloseDialog;
  ReplaceDialog1.Execute;

end;

procedure TForm1.ReplaceDialog1Replace(Sender: TObject);
var
  SelPos: Integer;
begin
   with TReplaceDialog(Sender) do
  begin
    { Perform a global case-sensitive search for FindText in Memo1. }
    SelPos := Pos(FindText, Memo1.Lines.Text);
    if SelPos > 0 then
    begin
      Memo1.SelStart := SelPos - 1;
      Memo1.SelLength := Length(FindText);
      Memo1.SetFocus;
      { Replace selected text with ReplaceText. }
      Memo1.SelText := ReplaceText;
    end else
      MessageDlg(Concat('Could not find "', FindText, '" in Memo1.'), mtError, [mbOk], 0);
  end;

end;

procedure TForm1.Run2Click(Sender: TObject);
begin
  Pint;
  Memo2.Lines.LoadFromFile(ChangeFileExt(NazevSouboru, '.out'));



end;

procedure TForm1.Save1Click(Sender: TObject);
begin
  SaveFile(false);
end;

procedure TForm1.SaveAs1Click(Sender: TObject);
begin
  SaveFile(true);
end;

end.

