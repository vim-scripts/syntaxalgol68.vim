" Vim syntax file
" Language:	Algol68
" Version: 0.1
" Last Change:	
" Maintainer:  NevilleD.ALGOL_68@sgr-a.net
" Previous Maintainer:	

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif


"syn case ignore
syn sync lines=250

" Algol68 Final Report, unrevised
syn keyword algol68ProProc	PRIORITY THEF
syn keyword algol68Operator	BTB CTB CONJ QUOTE CT CTAB EITHER SIGN


" Algol68 Revised Report
syn keyword algol68Boolean	TRUE FALSE
syn keyword algol68Conditional	IF THEN ELSE ELIF FI
syn keyword algol68Conditional	CASE IN OUT OUSE ESAC
syn keyword algol68Constant	NIL SKIP EMPTY
syn keyword algol68PreProc	MODE OP PRIO PROC
syn keyword algol68Label	GOTO 
syn match algol68label          "\<GO TO\>"
syn keyword algol68Operator	NOT UP DOWN LWB UPB
syn keyword algol68Operator	ABS ARG BIN ENTIER LENG LEVEL ODD REPR ROUND SHORTEN
syn keyword algol68Operator	SHL SHR  UP DOWN LWB UPB I
syn keyword algol68Operator	OVER MOD ELEM
syn keyword algol68Operator	LT LE GE GT
syn keyword algol68Operator	EQ NE
syn keyword algol68Operator	AND OR
syn keyword algol68PreProc	ANDF ORF ANDTH OREL ANDTHEN ORELSE
syn keyword algol68Operator	MINUSAB PLUSAB TIMESAB DIVAB OVERAB MODAB PLUSTO
syn keyword algol68Operator	IS ISNT OF AT
syn keyword algol68Repeat	FOR FROM BY UPTO DOWNTO TO WHILE DO OD
syn keyword algol68Statement	PAR BEGIN EXIT END 
syn keyword algol68Struct	STRUCT
syn keyword algol68PreProc	VECTOR
syn keyword algol68Type		FLEX HEAP LOC LONG REF SHORT
syn keyword algol68Type		BITS BOOL BYTES CHAR COMPL INT REAL SEMA STRING VOID
syn keyword algol68Type		CHANNEL FILE FORMAT STRUCT UNION

    " 20011222az: Added new items.
syn keyword algol68Todo contained	TODO FIXME XXX DEBUG NOTE

    " 20010723az: When wanted, highlight the trailing whitespace -- this is
    " based on c_space_errors; to enable, use "algol68_space_errors".
if exists("algol68_space_errors")
    if !exists("algol68_no_trail_space_error")
        syn match algol68SpaceError "\s\+$"
    endif
    if !exists("algol68_no_tab_space_error")
        syn match algol68SpaceError " \+\t"me=e-1
    endif
endif



" String
if !exists("algol68_one_line_string")
  syn region  algol68String matchgroup=algol68String start=+"+ end=+"+ contains=algol68StringEscape
  if exists("algol68_gpc")
    syn region  algol68String matchgroup=algol68String start=+'+ end=+'+ contains=algol68StringEscapeGPC
  else
    syn region  algol68StringError matchgroup=algol68StringError start=+'+ end=+'+ contains=algol68StringEscape
  endif
else
  "wrong strings
  syn region  algol68StringError matchgroup=algol68StringError start=+"+ end=+"+ end=+$+ contains=algol68StringEscape
  if exists("algol68_gpc")
    syn region  algol68StringError matchgroup=algol68StringError start=+'+ end=+'+ end=+$+ contains=algol68StringEscapeGPC
  else
    syn region  algol68StringError matchgroup=algol68StringError start=+'+ end=+'+ end=+$+ contains=algol68StringEscape
  endif

  "right strings
  syn region  algol68String matchgroup=algol68String start=+"+ end=+"+ oneline contains=algol68StringEscape
  " To see the start and end of strings:
  " syn region  algol68String matchgroup=algol68StringError start=+'+ end=+'+ oneline contains=algol68StringEscape
  if exists("algol68_gpc")
    syn region  algol68String matchgroup=algol68String start=+'+ end=+'+ oneline contains=algol68StringEscapeGPC
  else
    syn region  algol68StringError matchgroup=algol68StringError start=+'+ end=+'+ oneline contains=algol68StringEscape
  endif
end
syn match   algol68StringEscape		contained '""'
syn match   algol68StringEscapeGPC	contained "''"


syn match   algol68Identifier		"\<[a-z][a-z0-9_]*\>"


if exists("algol68_symbol_operator")
" syn match algol68SymbolOperator "₁₀"
  syn match algol68SymbolOperator "\\" 
"  syn match algol68SymbolOperator "≤" "<=" "≥" ">=" ">" "<"
"  syn match algol68SymbolOperator "≠" "/=" "~=" ":~=:" ":≠:" ":/=:" ":=:" ":=" ":=:="
""  syn match algol68SymbolOperator "[⍧¢£#]"
"  syn match algol68SymbolOperator "⌈" "⌊"
"  syn match algol68SymbolOperator "⎕"
"  syn match algol68SymbolOperator "[.][.]|‥"
"  syn match algol68SymbolOperator "¬" "~"
"  syn match algol68SymbolOperator "÷" "×"
"  syn match algol68SymbolOperator "⊥"
"  syn match algol68SymbolOperator "[○∅]"
"  syn match algol68SymbolOperator "↑" "↓"
"  syn match algol68SymbolOperator "∧" "∨" "&"
"  syn match algol68SymbolOperator "→"
"  syn match algol68SymbolOperator "[☩✠᛭]"
"  syn match algol68SymbolOperator "╭" "╰"
"9      +*, i 	+×, ⊥ 		
"8 	shl, shr, **, up, down, lwb, upb 	↑, ↓, ⌊, ⌈ 		╰, ╭
"7 	*, /, %, over, %*, mod, elem 	×, ÷, ÷×, ÷*, %×, ⌷ 		÷:
"6 	-, + 			
"5 	<, lt, <=, le, >=, ge, >, gt 	≤, ≥ 	
"4 	=, eq, /=, ne 	≠ 	~= 	
"3 	&, and 	∧ 	
"2 	or 	∨ 	
"1 	minusab, plusab, timesab, divab, overab, modab, plusto,
"
"-:=, +:=, *:=, /:=, %:=, %*:=, +=:
"	×:=, ÷:=, ÷×:=, ÷*:=, %×:= 		÷::=
"effectively 0 	 :=, =:, = , :=:, :/=:, is, isnt, of, at 	 :≠:, : 	 :~=: 	 ::, .., is not, →, @

"""" additional ALGOL characters/operators from html4 std
"  cent ¢    pound £  yen ¥    euro €    curren ¤
"  plusmn ±  minus −  times ×  divide ÷  oplus ⊕  otimes ⊗
"  larr ←    uarr ↑   rarr →   darr ↓    harr ↔   crarr ↵
"  and ∧     or ∨     not ¬
"  cong ≅    asymp ≈  prop ∝   ne ≠
"  empty ∅   infin ∞
"  perp ⊥    lceil ⌈  rceil ⌉  lfloor ⌊  rfloor ⌋
"  equiv ≡   sup ⊃    nabla ∇  loz ◊     deg °    # [[GOST 10859]] ALGOL characters
"  radic √   part ∂   int ∫    prod ∏    sum ∑    # bonus operators
""""

  syn match   algol68SymbolOperator      "[+\-/*=]"
"  syn match   algol68SymbolOperator      "[<>]=\="
  syn match   algol68SymbolOperator      "<>"
  syn match   algol68SymbolOperator      ":="
"  syn match   algol68SymbolOperator      "[()]"
  syn match   algol68SymbolOperator      "\.\."
"  syn match   algol68SymbolOperator       "[\^.]"
"  syn match   algol68MatrixDelimiter	"[][]"
  "if you prefer you can highlight the range
  syn match  algol68MatrixDelimiter	"[\d\+\.\.\d\+]"
endif

syn match  algol68Number		"-\=\<\d\+\>"
syn match  algol68Float		"-\=\<\d\+\.\d\+\>"
" add subscr 10
syn match  algol68Float		"-\=\<\d\+\.\d\+[eE\\]-\=\d\+\>" 
syn match  algol68Float		"-\=\<\d\+\.\d\+₁₀-\=\d\+\>" 
"syn match  algol68HexNumber	"\$[0-9a-fA-F]\+\>"
syn match  algol68HexNumber	"\<2r[01]\+\>"
syn match  algol68HexNumber	"\<4r[0-3]\+\>"
syn match  algol68HexNumber	"\<8r[0-7]\+\>"
syn match  algol68HexNumber	"\<16r[0-7a-f]\+\>"

if exists("algol68_no_tabs")
  syn match algol68ShowTab "\t"
endif


syn region algol68Special	start="\$"  end="\$" contains=algol68String
syn region algol68Comment	start="¢"  end="¢" contains=algol68Todo,algol68SpaceError
syn region algol68Comment	start="£"  end="£" contains=algol68Todo,algol68SpaceError
syn region algol68Comment	start="#"  end="#" contains=algol68Todo,algol68SpaceError
syn region algol68Comment	start="\<CO\>"  end="\<CO\>" contains=algol68Todo,algol68SpaceError
syn region algol68Comment	start="\<COMMENT\>"  end="\<COMMENT\>" contains=algol68Todo,algol68SpaceError
syn region algol68PreProc	start="\<PR\>"  end="\<PR\>" contains=algol68Todo,algol68SpaceError
syn region algol68PreProc	start="\<PRAGMAT\>"  end="\<PRAGMAT\>" contains=algol68Todo,algol68SpaceError
" algol68r
syn region algol68Comment	start="{"  end="}" contains=algol68Todo,algol68SpaceError
syn region algol68Comment	start="{{{"  end="}}}" contains=algol68Todo,algol68SpaceError

" ALGOL 68r
syn keyword algol68PreProc DECS CONTEXT configinfo A68CONFIG KEEP FINISH USE SYSPROCS IOSTATE FORALL
" ALGOL 68c
syn keyword algol68PreProc USING ENVIRON FOREACH ASSERT

if !exists("algol68_traditional")

"  THE STANDARD ENVIRONMENT

"      Enquiries
  syn match algol68Predefined "\<int lengths\>"
  syn match algol68Predefined "\<int shorths\>"
  syn match algol68Predefined "\<max int\>"
  syn match algol68Predefined "\<real lengths\>"
  syn match algol68Predefined "\<real shorths\>"
  syn match algol68Predefined "\<bits lengths\>"
  syn match algol68Predefined "\<bits shorths\>"
  syn match algol68Predefined "\<bytes lengths\>"
  syn match algol68Predefined "\<bytes shorths\>"
  syn match algol68Predefined "\<max abs char\>"
  syn match algol68Predefined "\<int width\>"
  syn match algol68Predefined "\<long int width\>"
  syn match algol68Predefined "\<long long int width\>"
  syn match algol68Predefined "\<real width\>"
  syn match algol68Predefined "\<long real width\>"
  syn match algol68Predefined "\<long long real width\>"
  syn match algol68Predefined "\<exp width\>"
  syn match algol68Predefined "\<long exp width\>"
  syn match algol68Predefined "\<long long exp width\>"
  syn match algol68Predefined "\<bits width\>"
  syn match algol68Predefined "\<long bits width\>"
  syn match algol68Predefined "\<long long bits width\>"
  syn match algol68Predefined "\<bytes width\>"
  syn match algol68Predefined "\<long bytes width\>"
  syn match algol68Predefined "\<max real\>"
  syn match algol68Predefined "\<small real\>"
  syn match algol68Predefined "\<long max int\>"
  syn match algol68Predefined "\<long long max int\>"
  syn match algol68Predefined "\<long max real\>"
  syn match algol68Predefined "\<long small real\>"
  syn match algol68Predefined "\<long long max real\>"
  syn match algol68Predefined "\<long long small real\>"
  syn match algol68Predefined "\<long max bits\>"
  syn match algol68Predefined "\<long long max bits\>"
  syn match algol68Predefined "\<null character\>"
  syn match algol68Predefined "\<error char\>"
  syn match algol68Predefined "\<exp char\>"
  syn match algol68Predefined "\<newline char\>"
  syn match algol68Predefined "\<formfeed char\>"
  syn match algol68Predefined "\<tab char\>"
  syn keyword algol68Predefined int_lengths intlengths int_shorths intshorths max_int maxint real_lengths reallengths real_shorths realshorths bits_lengths bitslengths bits_shorths bitsshorths bytes_lengths byteslengths bytes_shorths bytesshorths max_abs_char maxabschar int_width intwidth long_int_width longintwidth long_long_int_width longlongintwidth real_width realwidth long_real_width longrealwidth long_long_real_width longlongrealwidth exp_width expwidth long_exp_width longexpwidth long_long_exp_width longlongexpwidth bits_width bitswidth long_bits_width longbitswidth long_long_bits_width longlongbitswidth bytes_width byteswidth long_bytes_width longbyteswidth max_real maxreal small_real smallreal long_max_int longmaxint long_long_max_int longlongmaxint long_max_real longmaxreal long_small_real longsmallreal long_long_max_real longlongmaxreal long_long_small_real longlongsmallreal long_max_bits longmaxbits long_long_max_bits longlongmaxbits null_character nullcharacter blank flip flop error_char errorchar exp_char expchar newline_char newlinechar formfeed_char formfeedchar tab_char tabchar

"      Modes

"      Transput
  syn match algol68Predefined "\<stand in channel\>"
  syn match algol68Predefined "\<stand out channel\>"
  syn match algol68Predefined "\<stand back channel\>"
  syn match algol68Predefined "\<stand draw channel\>"
  syn match algol68Predefined "\<stand error channel\>"
  syn match algol68Function "\<put possible\>"
  syn match algol68Function "\<get possible\>"
  syn match algol68Function "\<bin possible\>"
  syn match algol68Function "\<set possible\>"
  syn match algol68Function "\<reset possible\>"
  syn match algol68Function "\<reidf possible\>"
  syn match algol68Function "\<draw possible\>"
  syn match algol68Function "\<on logical file end\>"
  syn match algol68Function "\<on physical file end\>"
  syn match algol68Function "\<on line end\>"
  syn match algol68Function "\<on page end\>"
  syn match algol68Function "\<on format end\>"
  syn match algol68Function "\<on value error\>"
  syn match algol68Function "\<on open error\>"
  syn match algol68Function "\<on transput error\>"
  syn match algol68Function "\<on format error\>"
  syn match algol68Function "\<new line\>"
  syn match algol68Function "\<write f\>"
  syn match algol68Function "\<print f\>"
  syn match algol68Function "\<write bin\>"
  syn match algol68Function "\<print bin\>"
  syn match algol68Function "\<read f\>"
  syn match algol68Function "\<read bin\>"
  syn match algol68Function "\<put f\>"
  syn match algol68Function "\<get f\>"
  syn match algol68Function "\<make term\>"
  syn match algol68Function "\<make device\>"
  syn match algol68Function "\<read int\>"
  syn match algol68Function "\<read long int\>"
  syn match algol68Function "\<read long long int\>"
  syn match algol68Function "\<read real\>"
  syn match algol68Function "\<read long real\>"
  syn match algol68Function "\<read long long real\>"
  syn match algol68Function "\<read complex\>"
  syn match algol68Function "\<read long complex\>"
  syn match algol68Function "\<read long long complex\>"
  syn match algol68Function "\<read bool\>"
  syn match algol68Function "\<read bits\>"
  syn match algol68Function "\<read long bits\>"
  syn match algol68Function "\<read long long bits\>"
  syn match algol68Function "\<read char\>"
  syn match algol68Function "\<read string\>"
  syn match algol68Function "\<print int\>"
  syn match algol68Function "\<print long int\>"
  syn match algol68Function "\<print long long int\>"
  syn match algol68Function "\<print real\>"
  syn match algol68Function "\<print long real\>"
  syn match algol68Function "\<print long long real\>"
  syn match algol68Function "\<print complex\>"
  syn match algol68Function "\<print long complex\>"
  syn match algol68Function "\<print long long complex\>"
  syn match algol68Function "\<print bool\>"
  syn match algol68Function "\<print bits\>"
  syn match algol68Function "\<print long bits\>"
  syn match algol68Function "\<print long long bits\>"
  syn match algol68Function "\<print char\>"
  syn match algol68Function "\<print string\>"
  syn keyword algol68Predefined stand_in_channel standinchannel stand_out_channel standoutchannel stand_back_channel standbackchannel stand_draw_channel standdrawchannel stand_error_channel standerrorchannel
  syn keyword algol68Function put_possible putpossible get_possible getpossible bin_possible binpossible set_possible setpossible reset_possible resetpossible reidf_possible reidfpossible draw_possible drawpossible compressible on_logical_file_end onlogicalfileend on_physical_file_end onphysicalfileend on_line_end onlineend on_page_end onpageend on_format_end onformatend on_value_error onvalueerror on_open_error onopenerror on_transput_error ontransputerror on_format_error onformaterror open establish create associate close lock scratch space new_line newline print write_f writef print_f printf write_bin writebin print_bin printbin read_f readf read_bin readbin put_f putf get_f getf make_term maketerm make_device makedevice idf term read_int readint read_long_int readlongint read_long_long_int readlonglongint read_real readreal read_long_real readlongreal read_long_long_real readlonglongreal read_complex readcomplex read_long_complex readlongcomplex read_long_long_complex readlonglongcomplex read_bool readbool read_bits readbits read_long_bits readlongbits read_long_long_bits readlonglongbits read_char readchar read_string readstring print_int printint print_long_int printlongint print_long_long_int printlonglongint print_real printreal print_long_real printlongreal print_long_long_real printlonglongreal print_complex printcomplex print_long_complex printlongcomplex print_long_long_complex printlonglongcomplex print_bool printbool print_bits printbits print_long_bits printlongbits print_long_long_bits printlonglongbits print_char printchar print_string printstring whole fixed float

"      Math constants
  syn match algol68Predefined "\<long pi\>"
  syn match algol68Predefined "\<long long pi\>"
  syn keyword algol68Predefined pi long_pi longpi long_long_pi longlongpi

"      Math procedures
  syn match algol68Function "\<arc sin\>"
  syn match algol68Function "\<arc cos\>"
  syn match algol68Function "\<arc tan\>"
  syn match algol68Function "\<long sqrt\>"
  syn match algol68Function "\<long curt\>"
  syn match algol68Function "\<long cbrt\>"
  syn match algol68Function "\<long exp\>"
  syn match algol68Function "\<long ln\>"
  syn match algol68Function "\<long log\>"
  syn match algol68Function "\<long sin\>"
  syn match algol68Function "\<long arc sin\>"
  syn match algol68Function "\<long cos\>"
  syn match algol68Function "\<long arc cos\>"
  syn match algol68Function "\<long tan\>"
  syn match algol68Function "\<long arc tan\>"
  syn match algol68Function "\<long long sqrt\>"
  syn match algol68Function "\<long long curt\>"
  syn match algol68Function "\<long long cbrt\>"
  syn match algol68Function "\<long long exp\>"
  syn match algol68Function "\<long long ln\>"
  syn match algol68Function "\<long long log\>"
  syn match algol68Function "\<long long sin\>"
  syn match algol68Function "\<long long arc sin\>"
  syn match algol68Function "\<long long cos\>"
  syn match algol68Function "\<long long arc cos\>"
  syn match algol68Function "\<long long tan\>"
  syn match algol68Function "\<long long arc tan\>"
  syn keyword algol68Function sqrt curt cbrt exp ln log sin arc_sin arcsin cos arc_cos arccos tan arc_tan arctan long_sqrt longsqrt long_curt longcurt long_cbrt longcbrt long_exp longexp long_ln longln long_log longlog long_sin longsin long_arc_sin longarcsin long_cos longcos long_arc_cos longarccos long_tan longtan long_arc_tan longarctan long_long_sqrt longlongsqrt long_long_curt longlongcurt long_long_cbrt longlongcbrt long_long_exp longlongexp long_long_ln longlongln long_long_log longlonglog long_long_sin longlongsin long_long_arc_sin longlongarcsin long_long_cos longlongcos long_long_arc_cos longlongarccos long_long_tan longlongtan long_long_arc_tan longlongarctan

"      Operator synonyms

"      Operator priorities

"      Monadic operators
  syn keyword algol68Predefined COMPLEX

"      Dyadic operators

"      Random number generator
  syn match algol68Function "\<first random\>"
  syn match algol68Function "\<next random\>"
  syn match algol68Function "\<long next random\>"
  syn match algol68Function "\<long long next random\>"
  syn keyword algol68Function first_random firstrandom next_random nextrandom long_next_random longnextrandom long_long_next_random longlongnextrandom

"      Miscellaneous
  syn match algol68Function "\<bits pack\>"
  syn match algol68Function "\<long bits pack\>"
  syn match algol68Function "\<long long bits pack\>"
  syn match algol68Function "\<bytes pack\>"
  syn match algol68Function "\<long bytes pack\>"
  syn match algol68Function "\<char in string\>"
  syn match algol68Function "\<last char in string\>"
  syn match algol68Function "\<string in string\>"
  syn match algol68Function "\<program idf\>"
  syn match algol68Function "\<cpu time\>"
  syn match algol68Function "\<sweep heap\>"
  syn match algol68Function "\<preemptive sweep heap\>"
  syn match algol68Function "\<collect seconds\>"
  syn match algol68Function "\<system stack size\>"
  syn match algol68Function "\<system stack pointer\>"
  syn match algol68Function "\<stack pointer\>"
  syn keyword algol68Function real bits_pack bitspack long_bits_pack longbitspack long_long_bits_pack longlongbitspack bytes_pack bytespack long_bytes_pack longbytespack char_in_string charinstring last_char_in_string lastcharinstring string_in_string stringinstring system break debug monitor evaluate program_idf programidf seconds clock cpu_time cputime sweep_heap sweepheap preemptive_sweep_heap preemptivesweepheap garbage collections collect_seconds collectseconds system_stack_size systemstacksize system_stack_pointer systemstackpointer stack_pointer stackpointer

"  UNIX EXTENSIONS

"      General definitions
  syn match algol68Function "\<utc time\>"
  syn match algol68Function "\<local time\>"
  syn match algol68Function "\<get env\>"
  syn match algol68Function "\<reset errno\>"
  syn keyword algol68Function utc_time utctime local_time localtime argc argv get_env getenv reset_errno reseterrno errno strerror

"      Processes
  syn match algol68Function "\<wait pid\>"
  syn match algol68Function "\<execve child\>"
  syn match algol68Function "\<execve output\>"
  syn match algol68Function "\<execve child pipe\>"
  syn keyword algol68Function fork wait_pid waitpid execve execve_child execvechild execve_output execveoutput execve_child_pipe execvechildpipe

"      Fetching web page contents and sending requests
  syn match algol68Function "\<http content\>"
  syn match algol68Function "\<tcp request\>"
  syn keyword algol68Function http_content httpcontent tcp_request tcprequest

"      Regular expressions in string manipulation
  syn match algol68Function "\<grep in string\>"
  syn match algol68Function "\<sub in string\>"
  syn keyword algol68Function grep_in_string grepinstring sub_in_string subinstring

"      Curses support
  syn match algol68Function "\<curses start\>"
  syn match algol68Function "\<curses end\>"
  syn match algol68Function "\<curses clear\>"
  syn match algol68Function "\<curses refresh\>"
  syn match algol68Function "\<curses getchar\>"
  syn match algol68Function "\<curses putchar\>"
  syn match algol68Function "\<curses move\>"
  syn match algol68Function "\<curses lines\>"
  syn match algol68Function "\<curses columns\>"
  syn keyword algol68Function curses_start cursesstart curses_end cursesend curses_clear cursesclear curses_refresh cursesrefresh curses_getchar cursesgetchar curses_putchar cursesputchar curses_move cursesmove curses_lines curseslines curses_columns cursescolumns

"  POSTGRESQL CLIENT ROUTINES

"      Connecting to a server
  syn match algol68Function "\<pq connect db\>"
  syn match algol68Function "\<pq finish\>"
  syn match algol68Function "\<pq reset\>"
  syn match algol68Function "\<pq parameter status\>"
  syn keyword algol68Function pq_connect_db pqconnectdb pq_finish pqfinish pq_reset pqreset pq_parameter_status pqparameterstatus

"      Sending queries and retrieving results
  syn match algol68Function "\<pq exec\>"
  syn match algol68Function "\<pq ntuples\>"
  syn match algol68Function "\<pq nfields\>"
  syn match algol68Function "\<pq fname\>"
  syn match algol68Function "\<pq fnumber\>"
  syn match algol68Function "\<pq fformat\>"
  syn match algol68Function "\<pq get is null\>"
  syn match algol68Function "\<pq get value\>"
  syn match algol68Function "\<pq cmd status\>"
  syn match algol68Function "\<pq cmd tuples\>"
  syn keyword algol68Function pq_exec pqexec pq_ntuples pqntuples pq_nfields pqnfields pq_fname pqfname pq_fnumber pqfnumber pq_fformat pqfformat pq_get_is_null pqgetisnull pq_get_value pqgetvalue pq_cmd_status pqcmdstatus pq_cmd_tuples pqcmdtuples

"      Connection status information
  syn match algol68Function "\<pq error message\>"
  syn match algol68Function "\<pq result error message\>"
  syn match algol68Function "\<pq db\>"
  syn match algol68Function "\<pq user\>"
  syn match algol68Function "\<pq passf) INT\>"
  syn match algol68Function "\<pq host\>"
  syn match algol68Function "\<pq port\>"
  syn match algol68Function "\<pq options\>"
  syn match algol68Function "\<pq protocol version\>"
  syn match algol68Function "\<pq server version\>"
  syn match algol68Function "\<pq socket\>"
  syn match algol68Function "\<pq backend pid\>"
  syn keyword algol68Function pq_error_message pqerrormessage pq_result_error_message pqresulterrormessage pq_db pqdb pq_user pquser pq_passf)_INT pqpassf)INT pq_host pqhost pq_port pqport pq_options pqoptions pq_protocol_version pqprotocolversion pq_server_version pqserverversion pq_socket pqsocket pq_backend_pid pqbackendpid

"      Behaviour in threaded programs

"  DRAWING USING THE GNU PLOTTING UTILITIES

"      Setting up a graphics device
  syn match algol68Function "\<draw device\>"
  syn match algol68Function "\<draw erase\>"
  syn match algol68Function "\<draw show\>"
  syn match algol68Function "\<draw move\>"
  syn match algol68Function "\<draw aspect\>"
  syn match algol68Function "\<draw fill style\>"
  syn match algol68Function "\<draw linestyle\>"
  syn match algol68Function "\<draw linewidth\>"
  syn keyword algol68Function draw_device drawdevice draw_erase drawerase draw_show drawshow draw_move drawmove draw_aspect drawaspect draw_fill_style drawfillstyle draw_linestyle drawlinestyle draw_linewidth drawlinewidth

"      Specifying colours
  syn match algol68Function "\<draw background colour\>"
  syn match algol68Function "\<draw background color\>"
  syn match algol68Function "\<draw background colour name\>"
  syn match algol68Function "\<draw background color name\>"
  syn match algol68Function "\<draw colour\>"
  syn match algol68Function "\<draw color\>"
  syn match algol68Function "\<draw colour name\>"
  syn match algol68Function "\<draw color name\>"
  syn keyword algol68Function draw_background_colour drawbackgroundcolour draw_background_color drawbackgroundcolor draw_background_colour_name drawbackgroundcolourname draw_background_color_name drawbackgroundcolorname draw_colour drawcolour draw_color drawcolor draw_colour_name drawcolourname draw_color_name drawcolorname

"      Drawing objects
  syn match algol68Function "\<draw point\>"
  syn match algol68Function "\<draw line\>"
  syn match algol68Function "\<draw rect\>"
  syn match algol68Function "\<draw circle\>"
  syn keyword algol68Function draw_point drawpoint draw_line drawline draw_rect drawrect draw_circle drawcircle

"      Drawing text
  syn match algol68Function "\<draw text\>"
  syn match algol68Function "\<draw text angle\>"
  syn match algol68Function "\<draw font name\>"
  syn match algol68Function "\<draw font size\>"
  syn keyword algol68Function draw_text drawtext draw_text_angle drawtextangle draw_font_name drawfontname draw_font_size drawfontsize

"  EXTRA NUMERICAL PROCEDURES

"      REAL functions
  syn match algol68Function "\<long sinh\>"
  syn match algol68Function "\<long long sinh\>"
  syn match algol68Function "\<arc sinh\>"
  syn match algol68Function "\<long arc sinh\>"
  syn match algol68Function "\<long long arc sinh\>"
  syn match algol68Function "\<long cosh\>"
  syn match algol68Function "\<long long cosh\>"
  syn match algol68Function "\<arc cosh\>"
  syn match algol68Function "\<long arc cosh\>"
  syn match algol68Function "\<long long arc cosh\>"
  syn match algol68Function "\<long tanh\>"
  syn match algol68Function "\<long long tanh\>"
  syn match algol68Function "\<arc tanh\>"
  syn match algol68Function "\<long arc tanh\>"
  syn match algol68Function "\<long long arc tanh\>"
  syn match algol68Function "\<arc tan2\>"
  syn match algol68Function "\<long arc tan2\>"
  syn match algol68Function "\<long long arc tan2\>"
  syn keyword algol68Function sinh long_sinh longsinh long_long_sinh longlongsinh arc_sinh arcsinh long_arc_sinh longarcsinh long_long_arc_sinh longlongarcsinh cosh long_cosh longcosh long_long_cosh longlongcosh arc_cosh arccosh long_arc_cosh longarccosh long_long_arc_cosh longlongarccosh tanh long_tanh longtanh long_long_tanh longlongtanh arc_tanh arctanh long_arc_tanh longarctanh long_long_arc_tanh longlongarctanh arc_tan2 arctan2 long_arc_tan2 longarctan2 long_long_arc_tan2 longlongarctan2

"      COMPLEX functions
  syn match algol68Function "\<complex sqrt\>"
  syn match algol68Function "\<long complex sqrt\>"
  syn match algol68Function "\<long long complex sqrt\>"
  syn match algol68Function "\<complex exp\>"
  syn match algol68Function "\<long complex exp\>"
  syn match algol68Function "\<long long complex exp\>"
  syn match algol68Function "\<complex ln\>"
  syn match algol68Function "\<long complex ln\>"
  syn match algol68Function "\<long long complex ln\>"
  syn match algol68Function "\<complex sin\>"
  syn match algol68Function "\<long complex sin\>"
  syn match algol68Function "\<long long complex sin\>"
  syn match algol68Function "\<complex arc sin\>"
  syn match algol68Function "\<long complex arc sin\>"
  syn match algol68Function "\<long long complex arc sin\>"
  syn match algol68Function "\<complex cos\>"
  syn match algol68Function "\<long complex cos\>"
  syn match algol68Function "\<long long complex cos\>"
  syn match algol68Function "\<complex arc cos\>"
  syn match algol68Function "\<long complex arc cos\>"
  syn match algol68Function "\<long long complex arc cos\>"
  syn match algol68Function "\<complex tan\>"
  syn match algol68Function "\<long complex tan\>"
  syn match algol68Function "\<long long complex tan\>"
  syn match algol68Function "\<complex arc tan\>"
  syn match algol68Function "\<long complex arc tan\>"
  syn match algol68Function "\<long long complex arc tan\>"
  syn match algol68Function "\<complex sinh\>"
  syn match algol68Function "\<complex arc sinh\>"
  syn match algol68Function "\<complex cosh\>"
  syn match algol68Function "\<complex arc cosh\>"
  syn match algol68Function "\<complex tanh\>"
  syn match algol68Function "\<complex arc tanh\>"
  syn keyword algol68Function complex_sqrt complexsqrt long_complex_sqrt longcomplexsqrt long_long_complex_sqrt longlongcomplexsqrt complex_exp complexexp long_complex_exp longcomplexexp long_long_complex_exp longlongcomplexexp complex_ln complexln long_complex_ln longcomplexln long_long_complex_ln longlongcomplexln complex_sin complexsin long_complex_sin longcomplexsin long_long_complex_sin longlongcomplexsin complex_arc_sin complexarcsin long_complex_arc_sin longcomplexarcsin long_long_complex_arc_sin longlongcomplexarcsin complex_cos complexcos long_complex_cos longcomplexcos long_long_complex_cos longlongcomplexcos complex_arc_cos complexarccos long_complex_arc_cos longcomplexarccos long_long_complex_arc_cos longlongcomplexarccos complex_tan complextan long_complex_tan longcomplextan long_long_complex_tan longlongcomplextan complex_arc_tan complexarctan long_complex_arc_tan longcomplexarctan long_long_complex_arc_tan longlongcomplexarctan complex_sinh complexsinh complex_arc_sinh complexarcsinh complex_cosh complexcosh complex_arc_cosh complexarccosh complex_tanh complextanh complex_arc_tanh complexarctanh

"      REAL Airy functions
  syn match algol68Function "\<airy ai\>"
  syn match algol68Function "\<airy bi\>"
  syn match algol68Function "\<airy ai derivative\>"
  syn match algol68Function "\<airy bi derivative\>"
  syn keyword algol68Function airy_ai airyai airy_bi airybi airy_ai_derivative airyaiderivative airy_bi_derivative airybiderivative

"      REAL Bessel functions
  syn match algol68Function "\<bessel jn\>"
  syn match algol68Function "\<bessel yn\>"
  syn match algol68Function "\<bessel in\>"
  syn match algol68Function "\<bessel exp in\>"
  syn match algol68Function "\<bessel kn\>"
  syn match algol68Function "\<bessel exp kn\>"
  syn match algol68Function "\<bessel jl\>"
  syn match algol68Function "\<bessel yl\>"
  syn match algol68Function "\<bessel exp il\>"
  syn match algol68Function "\<bessel exp kl\>"
  syn match algol68Function "\<bessel jnu\>"
  syn match algol68Function "\<bessel ynu\>"
  syn match algol68Function "\<bessel inu\>"
  syn match algol68Function "\<bessel exp inu\>"
  syn match algol68Function "\<bessel knu\>"
  syn match algol68Function "\<bessel exp knu\>"
  syn keyword algol68Function bessel_jn besseljn bessel_yn besselyn bessel_in besselin bessel_exp_in besselexpin bessel_kn besselkn bessel_exp_kn besselexpkn bessel_jl besseljl bessel_yl besselyl bessel_exp_il besselexpil bessel_exp_kl besselexpkl bessel_jnu besseljnu bessel_ynu besselynu bessel_inu besselinu bessel_exp_inu besselexpinu bessel_knu besselknu bessel_exp_knu besselexpknu

"      REAL Elliptic integrals
  syn match algol68Function "\<elliptic integral k\>"
  syn match algol68Function "\<elliptic integral e\>"
  syn match algol68Function "\<elliptic integral rf\>"
  syn match algol68Function "\<elliptic integral rd\>"
  syn match algol68Function "\<elliptic integral rj\>"
  syn match algol68Function "\<elliptic integral rc\>"
  syn keyword algol68Function elliptic_integral_k ellipticintegralk elliptic_integral_e ellipticintegrale elliptic_integral_rf ellipticintegralrf elliptic_integral_rd ellipticintegralrd elliptic_integral_rj ellipticintegralrj elliptic_integral_rc ellipticintegralrc

"      REAL Error and Gamma functions
  syn match algol68Function "\<inverse erf\>"
  syn match algol68Function "\<inverse erfc\>"
  syn match algol68Function "\<incomplete gamma\>"
  syn match algol68Function "\<ln gamma\>"
  syn match algol68Function "\<incomplete beta\>"
  syn keyword algol68Function erf inverse_erf inverseerf erfc inverse_erfc inverseerfc gamma incomplete_gamma incompletegamma ln_gamma lngamma factorial beta incomplete_beta incompletebeta

"      Physical Constants

"          Fundamental constants
  syn match algol68Predefined "\<mksa speed of light\>"
  syn match algol68Predefined "\<cgs speed of light\>"
  syn match algol68Predefined "\<mksa vacuum permeability\>"
  syn match algol68Predefined "\<mksa vacuum permittivity\>"
  syn match algol68Predefined "\<num avogadro\>"
  syn match algol68Predefined "\<mksa faraday\>"
  syn match algol68Predefined "\<cgs faraday\>"
  syn match algol68Predefined "\<mksa boltzmann\>"
  syn match algol68Predefined "\<cgs boltzmann\>"
  syn match algol68Predefined "\<mksa molar gas\>"
  syn match algol68Predefined "\<cgs molar gas\>"
  syn match algol68Predefined "\<mksa standard gas volume\>"
  syn match algol68Predefined "\<cgs standard gas volume\>"
  syn match algol68Predefined "\<mksa planck constant\>"
  syn match algol68Predefined "\<cgs planck constant\>"
  syn match algol68Predefined "\<mksa planck constant bar\>"
  syn match algol68Predefined "\<cgs planck constant bar\>"
  syn match algol68Predefined "\<mksa gauss\>"
  syn match algol68Predefined "\<cgs gauss\>"
  syn match algol68Predefined "\<mksa micron\>"
  syn match algol68Predefined "\<cgs micron\>"
  syn match algol68Predefined "\<mksa hectare\>"
  syn match algol68Predefined "\<cgs hectare\>"
  syn match algol68Predefined "\<mksa miles per hour\>"
  syn match algol68Predefined "\<cgs miles per hour\>"
  syn match algol68Predefined "\<mksa kilometers per hour\>"
  syn match algol68Predefined "\<cgs kilometers per hour\>"
  syn keyword algol68Predefined mksa_speed_of_light mksaspeedoflight cgs_speed_of_light cgsspeedoflight mksa_vacuum_permeability mksavacuumpermeability mksa_vacuum_permittivity mksavacuumpermittivity num_avogadro numavogadro mksa_faraday mksafaraday cgs_faraday cgsfaraday mksa_boltzmann mksaboltzmann cgs_boltzmann cgsboltzmann mksa_molar_gas mksamolargas cgs_molar_gas cgsmolargas mksa_standard_gas_volume mksastandardgasvolume cgs_standard_gas_volume cgsstandardgasvolume mksa_planck_constant mksaplanckconstant cgs_planck_constant cgsplanckconstant mksa_planck_constant_bar mksaplanckconstantbar cgs_planck_constant_bar cgsplanckconstantbar mksa_gauss mksagauss cgs_gauss cgsgauss mksa_micron mksamicron cgs_micron cgsmicron mksa_hectare mksahectare cgs_hectare cgshectare mksa_miles_per_hour mksamilesperhour cgs_miles_per_hour cgsmilesperhour mksa_kilometers_per_hour mksakilometersperhour cgs_kilometers_per_hour cgskilometersperhour

"          Astronomy and astrophysics
  syn match algol68Predefined "\<mksa astronomical unit\>"
  syn match algol68Predefined "\<cgs astronomical unit\>"
  syn match algol68Predefined "\<mksa gravitational constant\>"
  syn match algol68Predefined "\<cgs gravitational constant\>"
  syn match algol68Predefined "\<mksa light year\>"
  syn match algol68Predefined "\<cgs light year\>"
  syn match algol68Predefined "\<mksa parsec\>"
  syn match algol68Predefined "\<cgs parsec\>"
  syn match algol68Predefined "\<mksa grav accel\>"
  syn match algol68Predefined "\<cgs grav accel\>"
  syn match algol68Predefined "\<mksa solar mass\>"
  syn match algol68Predefined "\<cgs solar mass\>"
  syn keyword algol68Predefined mksa_astronomical_unit mksaastronomicalunit cgs_astronomical_unit cgsastronomicalunit mksa_gravitational_constant mksagravitationalconstant cgs_gravitational_constant cgsgravitationalconstant mksa_light_year mksalightyear cgs_light_year cgslightyear mksa_parsec mksaparsec cgs_parsec cgsparsec mksa_grav_accel mksagravaccel cgs_grav_accel cgsgravaccel mksa_solar_mass mksasolarmass cgs_solar_mass cgssolarmass

"          Atomic and nuclear physics
  syn match algol68Predefined "\<mksa electron charge\>"
  syn match algol68Predefined "\<cgs electron charge\>"
  syn match algol68Predefined "\<mksa electron volt\>"
  syn match algol68Predefined "\<cgs electron volt\>"
  syn match algol68Predefined "\<mksa unified atomic mass\>"
  syn match algol68Predefined "\<cgs unified atomic mass\>"
  syn match algol68Predefined "\<mksa mass electron\>"
  syn match algol68Predefined "\<cgs mass electron\>"
  syn match algol68Predefined "\<mksa mass muon\>"
  syn match algol68Predefined "\<cgs mass muon\>"
  syn match algol68Predefined "\<mksa mass proton\>"
  syn match algol68Predefined "\<cgs mass proton\>"
  syn match algol68Predefined "\<mksa mass neutron\>"
  syn match algol68Predefined "\<cgs mass neutron\>"
  syn match algol68Predefined "\<num fine structure\>"
  syn match algol68Predefined "\<mksa rydberg\>"
  syn match algol68Predefined "\<cgs rydberg\>"
  syn match algol68Predefined "\<mksa bohr radius\>"
  syn match algol68Predefined "\<cgs bohr radius\>"
  syn match algol68Predefined "\<mksa angstrom\>"
  syn match algol68Predefined "\<cgs angstrom\>"
  syn match algol68Predefined "\<mksa barn\>"
  syn match algol68Predefined "\<cgs barn\>"
  syn match algol68Predefined "\<mksa bohr magneton\>"
  syn match algol68Predefined "\<cgs bohr magneton\>"
  syn match algol68Predefined "\<mksa nuclear magneton\>"
  syn match algol68Predefined "\<cgs nuclear magneton\>"
  syn match algol68Predefined "\<mksa electron magnetic moment\>"
  syn match algol68Predefined "\<cgs electron magnetic moment\>"
  syn match algol68Predefined "\<mksa proton magnetic moment\>"
  syn match algol68Predefined "\<cgs proton magnetic moment\>"
  syn keyword algol68Predefined mksa_electron_charge mksaelectroncharge cgs_electron_charge cgselectroncharge mksa_electron_volt mksaelectronvolt cgs_electron_volt cgselectronvolt mksa_unified_atomic_mass mksaunifiedatomicmass cgs_unified_atomic_mass cgsunifiedatomicmass mksa_mass_electron mksamasselectron cgs_mass_electron cgsmasselectron mksa_mass_muon mksamassmuon cgs_mass_muon cgsmassmuon mksa_mass_proton mksamassproton cgs_mass_proton cgsmassproton mksa_mass_neutron mksamassneutron cgs_mass_neutron cgsmassneutron num_fine_structure numfinestructure mksa_rydberg mksarydberg cgs_rydberg cgsrydberg mksa_bohr_radius mksabohrradius cgs_bohr_radius cgsbohrradius mksa_angstrom mksaangstrom cgs_angstrom cgsangstrom mksa_barn mksabarn cgs_barn cgsbarn mksa_bohr_magneton mksabohrmagneton cgs_bohr_magneton cgsbohrmagneton mksa_nuclear_magneton mksanuclearmagneton cgs_nuclear_magneton cgsnuclearmagneton mksa_electron_magnetic_moment mksaelectronmagneticmoment cgs_electron_magnetic_moment cgselectronmagneticmoment mksa_proton_magnetic_moment mksaprotonmagneticmoment cgs_proton_magnetic_moment cgsprotonmagneticmoment

"          Time
  syn match algol68Predefined "\<mksa minute\>"
  syn match algol68Predefined "\<cgs minute\>"
  syn match algol68Predefined "\<mksa hour\>"
  syn match algol68Predefined "\<cgs hour\>"
  syn match algol68Predefined "\<mksa day\>"
  syn match algol68Predefined "\<cgs day\>"
  syn match algol68Predefined "\<mksa week\>"
  syn match algol68Predefined "\<cgs week\>"
  syn keyword algol68Predefined mksa_minute mksaminute cgs_minute cgsminute mksa_hour mksahour cgs_hour cgshour mksa_day mksaday cgs_day cgsday mksa_week mksaweek cgs_week cgsweek

"          Imperial units
  syn match algol68Predefined "\<mksa inch\>"
  syn match algol68Predefined "\<cgs inch\>"
  syn match algol68Predefined "\<mksa foot\>"
  syn match algol68Predefined "\<cgs foot\>"
  syn match algol68Predefined "\<mksa yard\>"
  syn match algol68Predefined "\<cgs yard\>"
  syn match algol68Predefined "\<mksa mile\>"
  syn match algol68Predefined "\<cgs mile\>"
  syn match algol68Predefined "\<mksa mil\>"
  syn match algol68Predefined "\<cgs mil\>"
  syn keyword algol68Predefined mksa_inch mksainch cgs_inch cgsinch mksa_foot mksafoot cgs_foot cgsfoot mksa_yard mksayard cgs_yard cgsyard mksa_mile mksamile cgs_mile cgsmile mksa_mil mksamil cgs_mil cgsmil

"          Nautical units
  syn match algol68Predefined "\<mksa nautical mile\>"
  syn match algol68Predefined "\<cgs nautical mile\>"
  syn match algol68Predefined "\<mksa fathom\>"
  syn match algol68Predefined "\<cgs fathom\>"
  syn match algol68Predefined "\<mksa knot\>"
  syn match algol68Predefined "\<cgs knot\>"
  syn keyword algol68Predefined mksa_nautical_mile mksanauticalmile cgs_nautical_mile cgsnauticalmile mksa_fathom mksafathom cgs_fathom cgsfathom mksa_knot mksaknot cgs_knot cgsknot

"          Volume
  syn match algol68Predefined "\<mksa acre\>"
  syn match algol68Predefined "\<cgs acre\>"
  syn match algol68Predefined "\<mksa liter\>"
  syn match algol68Predefined "\<cgs liter\>"
  syn match algol68Predefined "\<mksa us gallon\>"
  syn match algol68Predefined "\<cgs us gallon\>"
  syn match algol68Predefined "\<mksa canadian gallon\>"
  syn match algol68Predefined "\<cgs canadian gallon\>"
  syn match algol68Predefined "\<mksa uk gallon\>"
  syn match algol68Predefined "\<cgs uk gallon\>"
  syn match algol68Predefined "\<mksa quart\>"
  syn match algol68Predefined "\<cgs quart\>"
  syn match algol68Predefined "\<mksa pint\>"
  syn match algol68Predefined "\<cgs pint\>"
  syn keyword algol68Predefined mksa_acre mksaacre cgs_acre cgsacre mksa_liter mksaliter cgs_liter cgsliter mksa_us_gallon mksausgallon cgs_us_gallon cgsusgallon mksa_canadian_gallon mksacanadiangallon cgs_canadian_gallon cgscanadiangallon mksa_uk_gallon mksaukgallon cgs_uk_gallon cgsukgallon mksa_quart mksaquart cgs_quart cgsquart mksa_pint mksapint cgs_pint cgspint

"          Mass and weight
  syn match algol68Predefined "\<mksa pound mass\>"
  syn match algol68Predefined "\<cgs pound mass\>"
  syn match algol68Predefined "\<mksa ounce mass\>"
  syn match algol68Predefined "\<cgs ounce mass\>"
  syn match algol68Predefined "\<mksa ton\>"
  syn match algol68Predefined "\<cgs ton\>"
  syn match algol68Predefined "\<mksa metric ton\>"
  syn match algol68Predefined "\<cgs metric ton\>"
  syn match algol68Predefined "\<mksa uk ton\>"
  syn match algol68Predefined "\<cgs uk ton\>"
  syn match algol68Predefined "\<mksa troy ounce\>"
  syn match algol68Predefined "\<cgs troy ounce\>"
  syn match algol68Predefined "\<mksa carat\>"
  syn match algol68Predefined "\<cgs carat\>"
  syn match algol68Predefined "\<mksa gram force\>"
  syn match algol68Predefined "\<cgs gram force\>"
  syn match algol68Predefined "\<mksa pound force\>"
  syn match algol68Predefined "\<cgs pound force\>"
  syn match algol68Predefined "\<mksa kilopound force\>"
  syn match algol68Predefined "\<cgs kilopound force\>"
  syn match algol68Predefined "\<mksa poundal\>"
  syn match algol68Predefined "\<cgs poundal\>"
  syn keyword algol68Predefined mksa_pound_mass mksapoundmass cgs_pound_mass cgspoundmass mksa_ounce_mass mksaouncemass cgs_ounce_mass cgsouncemass mksa_ton mksaton cgs_ton cgston mksa_metric_ton mksametricton cgs_metric_ton cgsmetricton mksa_uk_ton mksaukton cgs_uk_ton cgsukton mksa_troy_ounce mksatroyounce cgs_troy_ounce cgstroyounce mksa_carat mksacarat cgs_carat cgscarat mksa_gram_force mksagramforce cgs_gram_force cgsgramforce mksa_pound_force mksapoundforce cgs_pound_force cgspoundforce mksa_kilopound_force mksakilopoundforce cgs_kilopound_force cgskilopoundforce mksa_poundal mksapoundal cgs_poundal cgspoundal

"          Thermal energy and power
  syn match algol68Predefined "\<mksa calorie\>"
  syn match algol68Predefined "\<cgs calorie\>"
  syn match algol68Predefined "\<mksa btu\>"
  syn match algol68Predefined "\<cgs btu\>"
  syn match algol68Predefined "\<mksa therm\>"
  syn match algol68Predefined "\<cgs therm\>"
  syn match algol68Predefined "\<mksa horsepower\>"
  syn match algol68Predefined "\<cgs horsepower\>"
  syn keyword algol68Predefined mksa_calorie mksacalorie cgs_calorie cgscalorie mksa_btu mksabtu cgs_btu cgsbtu mksa_therm mksatherm cgs_therm cgstherm mksa_horsepower mksahorsepower cgs_horsepower cgshorsepower

"          Pressure
  syn match algol68Predefined "\<mksa bar\>"
  syn match algol68Predefined "\<cgs bar\>"
  syn match algol68Predefined "\<mksa std atmosphere\>"
  syn match algol68Predefined "\<cgs std atmosphere\>"
  syn match algol68Predefined "\<mksa torr\>"
  syn match algol68Predefined "\<cgs torr\>"
  syn match algol68Predefined "\<mksa meter of mercury\>"
  syn match algol68Predefined "\<cgs meter of mercury\>"
  syn match algol68Predefined "\<mksa inch of mercury\>"
  syn match algol68Predefined "\<cgs inch of mercury\>"
  syn match algol68Predefined "\<mksa inch of water\>"
  syn match algol68Predefined "\<cgs inch of water\>"
  syn match algol68Predefined "\<mksa psi\>"
  syn match algol68Predefined "\<cgs psi\>"
  syn keyword algol68Predefined mksa_bar mksabar cgs_bar cgsbar mksa_std_atmosphere mksastdatmosphere cgs_std_atmosphere cgsstdatmosphere mksa_torr mksatorr cgs_torr cgstorr mksa_meter_of_mercury mksameterofmercury cgs_meter_of_mercury cgsmeterofmercury mksa_inch_of_mercury mksainchofmercury cgs_inch_of_mercury cgsinchofmercury mksa_inch_of_water mksainchofwater cgs_inch_of_water cgsinchofwater mksa_psi mksapsi cgs_psi cgspsi

"          Viscosity
  syn match algol68Predefined "\<mksa poise\>"
  syn match algol68Predefined "\<cgs poise\>"
  syn match algol68Predefined "\<mksa stokes\>"
  syn match algol68Predefined "\<cgs stokes\>"
  syn keyword algol68Predefined mksa_poise mksapoise cgs_poise cgspoise mksa_stokes mksastokes cgs_stokes cgsstokes

"          Light and illumination
  syn match algol68Predefined "\<mksa stilb\>"
  syn match algol68Predefined "\<cgs stilb\>"
  syn match algol68Predefined "\<mksa lumen\>"
  syn match algol68Predefined "\<cgs lumen\>"
  syn match algol68Predefined "\<mksa lux\>"
  syn match algol68Predefined "\<cgs lux\>"
  syn match algol68Predefined "\<mksa phot\>"
  syn match algol68Predefined "\<cgs phot\>"
  syn match algol68Predefined "\<mksa footcandle\>"
  syn match algol68Predefined "\<cgs footcandle\>"
  syn match algol68Predefined "\<mksa lambert\>"
  syn match algol68Predefined "\<cgs lambert\>"
  syn match algol68Predefined "\<mksa footlambert\>"
  syn match algol68Predefined "\<cgs footlambert\>"
  syn keyword algol68Predefined mksa_stilb mksastilb cgs_stilb cgsstilb mksa_lumen mksalumen cgs_lumen cgslumen mksa_lux mksalux cgs_lux cgslux mksa_phot mksaphot cgs_phot cgsphot mksa_footcandle mksafootcandle cgs_footcandle cgsfootcandle mksa_lambert mksalambert cgs_lambert cgslambert mksa_footlambert mksafootlambert cgs_footlambert cgsfootlambert

"          Radioactivity
  syn match algol68Predefined "\<mksa curie\>"
  syn match algol68Predefined "\<cgs curie\>"
  syn match algol68Predefined "\<mksa roentgen\>"
  syn match algol68Predefined "\<cgs roentgen\>"
  syn match algol68Predefined "\<mksa rad\>"
  syn match algol68Predefined "\<cgs rad\>"
  syn keyword algol68Predefined mksa_curie mksacurie cgs_curie cgscurie mksa_roentgen mksaroentgen cgs_roentgen cgsroentgen mksa_rad mksarad cgs_rad cgsrad

"          Force and energy
  syn match algol68Predefined "\<mksa newton\>"
  syn match algol68Predefined "\<cgs newton\>"
  syn match algol68Predefined "\<mksa dyne\>"
  syn match algol68Predefined "\<cgs dyne\>"
  syn match algol68Predefined "\<mksa joule\>"
  syn match algol68Predefined "\<cgs joule\>"
  syn match algol68Predefined "\<mksa erg\>"
  syn match algol68Predefined "\<cgs erg\>"
  syn keyword algol68Predefined mksa_newton mksanewton cgs_newton cgsnewton mksa_dyne mksadyne cgs_dyne cgsdyne mksa_joule mksajoule cgs_joule cgsjoule mksa_erg mksaerg cgs_erg cgserg
endif

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_algol68_syn_inits")
  if version < 508
    let did_algol68_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink algol68Acces		algol68Statement
  HiLink algol68Boolean		Boolean
  HiLink algol68Comment		Comment
  HiLink algol68Conditional	Conditional
  HiLink algol68Constant		Constant
  HiLink algol68Delimiter	Identifier
  HiLink algol68Directive	algol68Statement
  HiLink algol68Exception	Exception
  HiLink algol68Float		Float
  HiLink algol68Function		Function
  HiLink algol68Label		Label
  HiLink algol68MatrixDelimiter	Identifier
  HiLink algol68Modifier		Type
  HiLink algol68HexNumber		Number
  HiLink algol68Number		Number
  HiLink algol68Operator		Operator
  HiLink algol68Predefined	algol68Statement
  HiLink algol68PreProc		PreProc
  HiLink algol68Repeat		Repeat
  HiLink algol68SpaceError	Error
  HiLink algol68Statement	Statement
  HiLink algol68String		String
  HiLink algol68Format	        Special
  HiLink algol68StringEscape	Special
  HiLink algol68StringEscapeGPC	Special
  HiLink algol68StringError	Error
  HiLink algol68Struct		algol68Statement
  HiLink algol68SymbolOperator	algol68Operator
  HiLink algol68Todo		Todo
  HiLink algol68Type		Type
  HiLink algol68Unclassified	algol68Statement
  "  HiLink algol68Asm		Assembler
  HiLink algol68Error		Error
  HiLink algol68AsmKey		algol68Statement
  HiLink algol68ShowTab		Error

  delcommand HiLink
endif

let b:current_syntax = "algol68"

" vim: ts=8 sw=2
