%{
	/*******************************************************\
	*							*
	*  readbib.lex  (prototype v0.7)  19 February 1997	*
	*							*
	*  Paul Taylor <pt@dcs.qww.ac.uk>			*
	*  Department of Computer Science,			*
	*  Queen Mary and Westfield College,			*
	*  London E1 4NS					*
	*							*
	*  Parses BibTeX-format database files,			*
	*  correcting (and reporting) numerous common errors.	*
	*							*
	*  Needs "flex" not "lex".				*
	*							*
	\*******************************************************/

   /********************** INTERFACE ***************************\

	THIS TEXT ...			BECOMES THESE SUBROUTINE CALLS ...

   @article{label,		   openrecord("article","label",code,line);
     author = {Joe Bloggs},	   setfield("author","Joe Bloggs");
     title = {A Brilliant Paper}   setfield("title","A Billiant Paper");
     note ={reprinted in \cite{A}} setfield("note","reprinted in \cite{A}");
				   citation("A");
   }				   closerecord(line);

   %  bibliography{		   openrecord("bibliography",0,code,line);
   %    author = {Joe Bloggs},	   setfield("author","Joe Bloggs");
   %    telephone = {555 3589}	   setfield("telephone","555 3589");
   %  }				   closerecord(line);

   @comment{			   bibcomment("@comment{");
     some {balanced} text	   bibcomment("some {balanced} text");
   }				   bibcomment("}");

   % other comments		   bibcomment("other comments");
   any junk between records	   bibcomment("any junk between records");

   @preamble{			   openrecord("preamble",0,code,line);
     "a {\TeX} preamble"	   setpreamble("a {\TeX} preamble",code,line);
   }				   closerecord(line);

   @string{			   openrecord("string",0,code,line);
     SV = "Springer-Verlag",	   setmacro("sv","Springer-Verlag",code,line);
     NH = "North-Holland"	   setmacro("nh","North-Holland",code,line);
   }				   closerecord(line);

   Record types, field names and string names are converted
   to lower case; labels and field values keep their capitals.
   Multiple spaces within field values are compressed, but
   the line-breaking stays the same.

   "code" is a bit-pattern: see #include readbib.h
   "line" is the line number in the source file

   setfield(keyword,nconcats,concat,concattype);
   char *concat[nconcats]; array of strings
   int concattype[nconcats]; array of numbers 1=text, 2=number, 3=macro

   The following subroutines are also called during parsing:

   is_structured_comment(name); should return 1 if "name" is a known
	structured comment type and 0 otherwise.
   biberr(severity,message,detail); reports errors of severity as follows
        3 = output may not contain all/valid data
	2 = would cause errors in BibTeX or LaTeX
	1 = something has been changed in the database

   \**********************************************************/

   /********************** FEATURES ***************************\

     *	Catches missing @ = comma and record terminator.
     *  Only treats @ as the beginning of a record if it's at the
	beginning of the line (apart from white space).
     *	Catches many (but not all) cases of missing string delimiters.
     *	Translates 8-bit characters and some HTML accents into TeX
	accents	(since & is used for matrices in TeX, which are
	unlikely to occur in bibliographies).
     *	Ensures that all TeX accents are enclosed in braces
	(otherwise G\"odel and Ad\'amek become [G\"] and [Ad\] with
	alpha.bst, which causes a mess in LaTeX).

   \**********************************************************/

   /********************** BUGS and THINGS TO DO ***************\

	should translate @ in comments to " at "
	389, 457, 461 Translate @ in comments and malformed records.

	Generate standardised labels.
	generate a dummy label if it's missing
	zero-length labels
	464 Generate missing citation key.

	Detect extra brace in @unpublished({Streett1,
	622 Extra brace before citation key.

	Detect authors separated by commas instead of "and"s.
	Parse authors' names into "first",  "von",  "surname" and "jr"
	169 Too many commas in name 1 of "K. Broda, S. Eisenbach,
	H. Khoshnevisan, S.J. Vickers" for entry VickersSJ:reasprog

	catch unterminated field values (recognise @...)
	726 Warning--you've exceeded 1000, the global-string-size,
	for entry TaylorP:towuti

	1105 Warning--all relevant fields are empty in test1996.bis

	Print a banner to stdout and sterr.

	Detect mis-matched outer brackets around a record.

	Detect repeated fields and citation keys.

	Detect non-standard TeX macros.

	Unbraced umlaut in quoted string is fatal in BibTeX.

	Makes a mess of "*-autonomous categories"

	"Universit\¬at Hannover" -> "Universit\{\lnot}at Hannover"

	double hyphens in page ranges

	catch		       filename = "{WaltersRFC.bib",

	catch missing } after structured comment

	catch  title={Denotational Semantics}  % missing comma

	ignore " which is not followed by a valid field

	detect embedded urls and "available by anonymous ftp"

	extra fields after blank lines

	@article{...}@comment{was Dijk1}

	recognise . and ; instead of comma

	catch Publisher = "DAIMI PB-397-II}", 

	@String{AmerSocio = "A. J. Soc" } % didn't find  = comment in record

	embedded quotes in quoted fields

	field names containing spaces

	recognise v@string{lncs="Lecture Notes in Computer Science"} as record

	Only allowed one string in a @string record

	BibTeX re-paragraphs, so TeX % comments don't work.

	@article
	{
	   label,
	isn't parsed.

	putting braces around \"U also forces it to be a capital

   \**********************************************************/

	/* FLEX regular expression definitions.
	 * "identifier" is the format of record-types and fieldnames
	 *	e.g.  book, article, etc. and author, title, etc.
	 * "label" is the bibliographic search key,  e.g. TaylorP:praf
	 * "opening" is used as a trailing context to recognise
	 *	"book{TaylorP:praf," as the beginning of a record.
	 * "extrafield" is similarly used to detect that another
	 *	field follows on the *next* line of the input.
	 * "ok" means being strict about the characters allowed
	 * The other patterns are for optional/mandatory white space,
	 * possibly with newlines or in structured comments.
	 */
%}
identifier	([^ \t\n\000-\020\177-\377%@"'`{}()#,=]*)
label		([^ \t\n\000-\020\177-\377"{}(),]*)
okidentifier	([A-Za-z][-A-Za-z]*)
oklabel		([A-Za-z][-0-9A-Za-z&_+/:.]*)
begin		[{(]
end		[})]
w		([ \t]*)
sp		([ \t]+)
wnl		([ \t\n]*)
spnl		([ \t\n]+)
blankline	([ \t]*\n[% \t]*\n)
pcwnl		({w}(\n{w}%+{w})*)
pcspnl		({sp}|{w}(\n{w}%+{w})+)
opening		{identifier}{wnl}{begin}{wnl}{label}{wnl},
okopening	{okidentifier}{wnl}{begin}{wnl}{oklabel}{wnl},
extrafield	{w}\n{w}%*{w}{okidentifier}{w}=
%s type open pcopen label pcent body equals value token quoted 
%s comma text braced undelim comment bracecomment nolabel preamble cite
%{
#include <strings.h>
#include <ctype.h>
/* NEST(s) and UNNEST push and pop the parsing state */
#define NEST(s) nested[nesting++]=YY_START; BEGIN(s)
#define UNNEST BEGIN(nested[--nesting])
/* These bits describe the current record and are accumulated in "code". */
#include "readbib.h"
	char valuebuffer[10000]; /* buffer for pending field value */
	char *thevalue=valuebuffer;
	char *concat[100]; /* pointers to concatenation components */
	int concattype[100]; /* their types */
	int nconcats=0; /* number of concatenation components */
	char junk[10000]; /* scratch string space */
	char savedcomments[10000]; /* comments found on this line */
	int nesting=0; /* parsing level */
	int nested[100]; /* parsing stack */
	int separator=0; /* add 1=space 2=\n 3=\n\n in pending field value */
	int input_line_number=1;
	int begin_at_line=0;
	char record_opener='\0';
	char keyword[10000]; /* pending field name (or record type) */
	int code=0, inrecord=0;
%}
%%
	/* initialisation code */
	char *getmacro();
	char expect='\0'; /* terminator for pending field value */
%{
	/****** ignore white space between tokens ******/
%}
{sp}				{ if (separator==0) {separator=1; }}
<pcent>{w}\n			{ inc_line_no(); UNNEST; }
<pcent>({w}\n)+\n		{ inc_line_no(); separator=3; UNNEST; }
{w}\n				{ inc_line_no();
				  if (separator<2) {separator=2; }}
({w}\n)+\n			{ inc_line_no(); separator=3; }
%{
	/****** <INITIAL> look for the beginning of a record ******
			The way BibTeX works is this:

	  Contrary to popular belief, BibTeX treats neither % nor
	  anything else as a *beginning* of comment character:
	  *everything* is a comment until BibTeX encounters an @ character
	  (*end* of comment, if you like), when it begins parsing
	  a database entry. This consists of a type (article, book, etc)
	  followed by a {}-balanced string. It doesn't (in the first
	  instance) care what's inside the string, and neither @ nor %
	  has any special significance there.  Everything after the }
	  matching the } in the beginning of the record is treated
	  as a comment again.  (Note, however, that % is used to begin
	  comments in .bst programs).

			What this program does:

	  It is customary for database entries to begin on a new line,
	  i.e. @ is the first character apart from white space.
	  If an @ after visible text on a line outside a record then
	  we treat it as an ordinary character in the comment, but a
	  warning is given; to make it significant, insert a newline.
	  On the other hand we also recognise the file header, begining
	  "% BibTeX-file{" (similarly "bibliography" and "control") as a
	  "structured comment"; if an @ is included then this is accepted
	  but a warning is given.
	  A well-formed beginning of record is also recognised even when
	  either the @ or the closing } of the previous record is missing
	  (but not both).
	  A } or ) after a number or macro string is ignored (not treated
	  as the end of the record) if it appears to be followed by another
	  valid field assignment.

	  <INITIAL>	outside record, open white space so far on this line
	  <type>	reading the type of a new record
	  <open>	expecting { or ( to open a new record
	  <pcopen>	ditto, in a structured comment
	  <label>	reading the label of a new record
	  <pcent>	treating this line as a comment
	 */
%}
	/* normal beginning of record, with @, type, label and comma */
<INITIAL>^{w}@{w}/{opening}	{ code=HAVE_AT;
				  begin_at_line=input_line_number;
                                  BEGIN type; }
	/* either the label or the comma is missing */
<INITIAL>^{w}@{w}/{okidentifier}{wnl}{begin} { code=HAVE_AT;
				  begin_at_line=input_line_number;
                                  BEGIN type; }
	/* the @ is missing */
<INITIAL>^{w}/{okopening}	{ code=0;
				  begin_at_line=input_line_number;
                                  BEGIN type; }
	/* the @ is not at the beginning of the line */
<INITIAL>@{w}/{okopening}	{ code=HAVE_AT;
				  begin_at_line=input_line_number;
				biberr(1,"record should begin on a new line","");
                                  BEGIN type; }
	/* an @ , but another record is unfinished */
^{w}@{w}/{okopening}		{ curtail(); 
			biberr(3,"missing } inserted between records","");
				  code=HAVE_AT;
				  begin_at_line=input_line_number;
				  BEGIN type;}
	/* structured comment with both % and @ */
<INITIAL>^{w}%+{w}@{w}/{okidentifier}{pcwnl}{begin} {
				code=HAVE_AT|HAVE_PCENT;
				  begin_at_line=input_line_number;
				BEGIN type;}
	/* structured comment with just % */
<INITIAL>^{w}%+{w}/{okidentifier}{pcwnl}{begin} { code=HAVE_PCENT; BEGIN type;}
<INITIAL>@			{ biberr(3,"malformed record - @ ignored","");
				  yymore(); NEST(pcent); }
%{
	/* read and recognised the type,
	   ignoring unrecognised structured comments */
%}
<type>{identifier}		{ recognise_type();
				  if (YY_START==INITIAL)
					 {yymore(); NEST(pcent);}
				}
%{
	/* read the label, if any, and open the record */
%}
<pcopen>{begin}{pcwnl}/{oklabel}{pcwnl}[,})] |
<open>{begin}{wnl}/{label}{wnl}[,})]  |
<pcopen>{begin}{pcwnl}/{oklabel}{extrafield} |
<open>{begin}{wnl}/{oklabel}{extrafield} {
				  record_opener=yytext[0];
				  inc_line_no(); BEGIN label;}
<open,pcopen>{label}		{ biberr(3,"missing open brace on",keyword);
				  doopenrecord(yytext);
				  BEGIN comma; }
<label>{label}			{ doopenrecord(yytext); BEGIN comma; }
<nolabel,open,pcopen>{begin} { code |= NO_LABEL;
				  if (code&WANT_LABEL)
				    {biberr(3,"missing label for",keyword);}
				  inc_line_no();
				  doopenrecord(0);
				  if (code&IS_PREAMBLE) {
					resetvalue(); BEGIN preamble;}
				  else if (code&IS_COMMENT) {
					BEGIN comment;}
				  else { BEGIN body; }
				}
<type,open,pcopen,label,nolabel><<EOF>>	{
				  biberr(3,"EOF while opening record","");
				  yyterminate(); }
%{
	/* comments */
%}
<INITIAL>^{w}%+			{ NEST(pcent); }
<text,braced,quoted>^{w}%+	{ if ((code&HAVE_PCENT)==0) {
				      addvalue(yytext);
				}}
<text,braced,quoted>%+		{ addvalue(yytext); }
^{w}%+				{ if ((code&HAVE_PCENT)==0) {
				      biberr(2,"comments within record","");
				      NEST(pcent);
				}}
\%+				{ if ((code&HAVE_PCENT)==0)
				      { biberr(2,"comments within record","");}
				      NEST(pcent);
				}
<INITIAL>{okidentifier}{w}={w}\"[^\n{}"%@=#]*\"{w}[,)}]{w}$ |
<INITIAL>{okidentifier}{w}={w}[{(][^\n{}%@=#]*[})]{w}[,)}]{w}$ |
<INITIAL>{okidentifier}{w}={w}{okidentifier}{w}[,)}]{w}$ |
<INITIAL>{okidentifier}{w}={w}[0-9]+{w}[,)}]{w}$ {
				  biberr(3,"possible data in comments",yytext);
				  savecomment(yytext); }
<INITIAL>[^ \t\n%@][^@\n]*	{ biberr(1,"unmarked comments",yytext);
				  savecomment(yytext); }
<INITIAL>[^ \t\n%@][^@\n]*/@{w}{okopening} { savecomment(yytext); }
<INITIAL>[^ \t\n%@].*		{ biberr(3,"@ in unmarked comments",yytext);
				  savecomment(yytext); }
<pcent>[^@\n]*			{ savecomment(yytext); UNNEST; }
<pcent>.*			{ biberr(2,"@ passed in comments",yytext);
				  savecomment(yytext); UNNEST; }
<comment,bracecomment>[^{}()\n]* { savecomment(yytext); }
<comment,bracecomment>{begin}	{ savecomment(yytext); NEST(bracecomment); }
<bracecomment>{end}		{ savecomment(yytext); UNNEST; }
<comment>{end}			{ docloserecord(); BEGIN INITIAL; }
%{
	/****** the body of a record ******
	  <body>   waiting for a field name
	  <equals> have field name, waiting for =
	  <value>  have =, waiting for the value
	  <token>  have numeric or string value, catch extra } before...
	  <quoted> have value, waiting for # , or }
	  <comma>  expecting , after the label
	  <preamble> reading @preamble, similar to <value>
	  */
%}
	/* get the name of the next field */
<comma>,			{ BEGIN body; }
<comma>[.;:]/{extrafield}  { biberr(2,"comma expected","");
				  BEGIN body; }
<comma,body,equals,value,token,quoted>[)}]{w},{wnl}/{extrafield} {
				  biberr(3,"record terminator ignored","");
				  dosetfield();
				  inc_line_no(); BEGIN body; }
<comma>{end}			{ match_block();
				  docloserecord();
                                  BEGIN INITIAL; }
<body>{identifier}		{ /* the field name */
				  copylower(keyword,yytext);
				  resetvalue();
				  BEGIN equals;  }
	/* the field name is missing */
<body,token,quoted>=		{ if (YY_START!=body)
				     {dosetfield(); }
				  biberr(3,"missing field name","");
				  strcpy(keyword,"data");
				  resetvalue();
				  BEGIN value;}				  
	/* unexpacted field name: missing comma or # */
<comma,token,quoted>{okidentifier} { if (YY_START!=comma) {UNNEST;}
				  /* changes YY_START to one of: */
				  switch (YY_START) {
				   case value:
					dosetfield();
					/* drop through */
				   case comma:
					copylower(keyword,yytext);
				  biberr(2,"missing comma before field",keyword);
					resetvalue();
					BEGIN equals;
					break;
				   case preamble:
					biberr(3,"missing # in @preamble","");
					addmacro(yytext);
					NEST(token);
				}}
<equals>=			{ BEGIN value;}
%{
	/******** Read the value of the field. ********
	* We use NEST to remember whether this is an ordinary field
	* or a @preamble.  After delimited text we go to "quoted",
	* after macros or numbers to "token" -- this helps to spot
	* missing or extra delimiters on the record.
	*/
%}
	/* a number (or undelimited range or pages or years) */
<value,preamble>[0-9]+		{ addvalue(yytext); NEST(token);}
<equals>[0-9]+/{wnl}[,})#]	{ biberr(2,"equals expected after",keyword);
				  addvalue(yytext); BEGIN value; NEST(token);}
<value,preamble>[0-9]+-+[0-9]+/{wnl}[,})#] {
				  biberr(2,"undelimited number range",yytext);
				  addvalue(yytext); NEST(token);}
	/* a macro (previously defined by @string) */
<value,preamble>{identifier}	{ addmacro(yytext);
				  NEST(token);}
<equals>{okidentifier}/{wnl}[,})#] { biberr(3,"equals expected after",keyword);
				  addmacro(yytext);
				  BEGIN value; NEST(token);}
	/* opening quotes or brackets */
<value,preamble>\"		{expect='"'; NEST(text);}
<value,preamble>\(		{expect=')'; NEST(text);}
<value,preamble>\{		{expect='}'; NEST(text);}
<equals>\"/[^\n\"@#=,{}]*\"{wnl}[,})#] { biberr(2,"equals expected after",keyword);
				 expect='"'; BEGIN value; NEST(text);}
<equals>\{/[^\n{}]*\}{wnl}[,})#] { biberr(2,"equals expected after",keyword);
				 expect='}'; BEGIN value; NEST(text);}
<equals>\(/[^\n{}]*\){wnl}[,})#] { biberr(2,"equals expected after",keyword);
				 expect=')'; BEGIN value; NEST(text);}
<value,preamble>['`]+		{ biberr(2,"funny open quotes at",keyword);
				  expect='"'; NEST(text);}
	/* ahead we can see a close-quote and another field */
<value>[^ \t\n{}()=#"'`\n%@][^{}()=#"'`\n%@]*[")}]/{w},{extrafield} {
				  expect=yytext[yyleng-1];
				  if (expect==')') {expect='(';}
				  if (expect=='}') {expect='{';}
				  strcpy(junk,yytext); junk[yyleng-1]='\0';
				  addvalue(junk);
				  sprintf(junk,"%c added before",expect);
				  biberr(3,junk,yytext);
				  NEST(quoted);}
	/* after part of the field value, a concatenation operator */
<token,quoted>#			{ concatop();
				  UNNEST; }
	/* the whole field value is terminated by comma or bracket */
<token,quoted>[.;:]/{extrafield}  |
<token,quoted>,			{ UNNEST;
				  if (YY_START==preamble)
			 	{biberr(3,"comma in preamble treated as #","");
				  concatop();
				}
				  else {
				  if (yytext[0]!=',')
				    { biberr(2,"comma expected",""); }
				 dosetfield(); BEGIN body; }}
	/* ignore bracket followed by another field or bracket */
<token>{end}/{wnl}{end}	|
<token>{end}{w}/,{extrafield}  {  biberr(3,"} ignored after",keyword); }
%{
	/****** delimited field values ******/
%}
<text>['`]+/{w},{extrafield}  |
<text>['`]+/{wnl}{end}{blankline}	{if (expect=='"')
				 { biberr(2,"funny close quotes at",keyword);
				   BEGIN quoted;}
				 else {addvalue(yytext);}
				}
<text>['`]+/{pcwnl}{end}{blankline} {if (expect=='"' && code&HAVE_PCENT)
				 { biberr(2,"funny close quotes at",keyword);
				   BEGIN quoted;}
				 else {addvalue(yytext);}
				}
<text>[")]			{if (expect==yytext[0]) { BEGIN quoted;}
				 else {addvalue(yytext);}
				}
<text>\\?\}			{ if (expect==yytext[0]) { BEGIN quoted;}
				  else
			   { biberr(3,"missing terminator for field",keyword);
				     match_block();
				     docloserecord();
                                     BEGIN INITIAL;}
				}
<text,braced>\\?\{		{ addvalue(yytext); NEST(braced);}
<braced>\\?\}			{ addvalue(yytext); UNNEST; }
<text>\"/[^,={}"@#%]*\"{wnl}[,})] |
<text>\"/[^,={}"@#]*\"{pcwnl}[,})] { if (expect==yytext[0]) {
			      biberr(3,"embedded quote in string ignored","");
				  } else {
				    addvalue(yytext);
				  }}
<text,braced>\\@		|
<text,braced>@			{ if (!(code&HAVE_AT))
		       {biberr(2, "changed @ to \" at \" in field",keyword);}
				  addvalue(" at ");}
%{
	/****** Comma at end of line, with a complete field
		on the next line: looks fishy!  ******/
%}
<text>,?/{extrafield}{w}\"[^={}()%@#\n]*\" {
				  if (expect=='\"') {
				     inc_line_no();
				     biberr(3,"missing \" after field",keyword);
				     strcpy(junk,yytext);
				     if(junk[yyleng-1]==',')
					{junk[yyleng-1]= '\0';}
				     addvalue(junk);
				     dosetfield();
				     BEGIN body; }
				  else { addvalue(yytext);}}
%{
	/****** 8-bit chars, HTML accents and unbraced TeX accents ******/
%}
<text,braced>[\200-\377]	{ numaccent((unsigned)(yytext[0]),
					"8-bit character"); }
<text,braced>&#[012][0-9][0-9];	{ numaccent(
					((unsigned)(yytext[2]-'0'))*100+
					((unsigned)(yytext[3]-'0'))*10+
					((unsigned)(yytext[4]-'0')),
					"HTML accent"); }
<text,braced>&[AEIOUYaeiouy]acute; {htmlaccent("\\\'",yytext[1]);}
<text,braced>&[AEIOUaeiou]circ;    {htmlaccent("\\^", yytext[1]);}
<text,braced>&[AEIOUaeiou]grave;   {htmlaccent("\\`", yytext[1]);}
<text,braced>&[ANOano]tilde;	   {htmlaccent("\\~", yytext[1]);}
<text,braced>&[AEIOUYaeiouy]uml;   {htmlaccent("\\\"",yytext[1]);}
<text,braced>&[Cc]cedil;	   {htmlaccent("\\c ",yytext[1]);}
<text,braced>&[Aa]ring;		   |
<text,braced>&[Oo]slash;	   {htmlaccent("\\",  yytext[1]);}
<text,braced>&lt;		{htmlsym("<");}
<text,braced>&gt;		{htmlsym(">");}
<text,braced>&amp;		{htmlsym("\\&");}
<text,braced>&quot;		{htmlsym("\"");}
<text,braced>&nbsp;		{htmlsym("~");}
<text,braced>&szlig;		{htmlsym("\\ss ");}
<text,braced>&AElig;		{htmlsym("\\AE ");}
<text,braced>&aelig;		{htmlsym("\\ae ");}
<text,braced>&Aring;		{htmlsym("\\A ");}
<text,braced>&aring;		{htmlsym("\\a ");}
<text,braced>&Oslash;		{htmlsym("\\O ");}
<text,braced>&oslash;		{htmlsym("\\o ");}
<text>\\[Hbcdtuv'`.=~^"]{w}\{([a-zA-Z]|\\[a-zA-Z]+{w}|\\[^a-zA-Z])\} 	{
				  biberr(1,"inserted {} around",yytext);
				  { char ch=yytext[1];
				    char *letter=index(yytext,'{')+1;
				  sprintf(junk,"{\\%c%s%s",ch,
	       (((letter[0]!='\\')&&(ch=='H'||('a'<=ch&&ch<='z')))?" ":""),
					letter);
				  addvalue(junk);}}
<text>\\['`.=~^"]{w}[^ \t\n"{}()\\]		|
<text>\\[Hbcdtuv]{sp}[a-zA-Z]			|
<text>\\[Hbcdtuv]{w}[^a-zA-Z \t\n"{}()\\]	|
<text>\\['`.=~^"Hbcdtuv]{w}\\[a-zA-Z]+{w}/[^a-zA-Z] |
<text>\\['`.=~^"Hbcdtuv]{w}\\[^a-zA-Z \t\n]+/[^a-zA-Z] |
<text>\\(oe|ae|aa|ss|OE|AE|AA|[ijolOL]){w}/[^a-zA-Z] {
				  biberr(1,"inserted {} around",yytext);
				  sprintf(junk,"{%s}",yytext);
				  addvalue(junk); }
<text>\\['`.=~^]		|
<text>\\[Hbcdtuv]/[^a-zA-Z]	{
				  biberr(1,"missing letter for accent",yytext);
				  sprintf(junk,"{%s{}}",yytext);
				  addvalue(junk); }
%{
	/****** embedded citations ******/
%}
<text,braced>\\cite{wnl}(\[[^][{}"\n]*\]{wnl})?\{{w}/{oklabel}({w},{oklabel})*{w}\} {
				  inc_line_no(); addvalue(yytext);
				  NEST(cite);
				}
<cite>{label}			{ citation(yytext); addvalue(yytext); }
<cite>,				{ addvalue(yytext); }
<cite>\}			{ addvalue(yytext); UNNEST; }
%{
	/****** copy other text ******/
%}
<text,braced>\\/[ \t\n]		{addvalue("\\ ");}
<text,braced>\\[-|_!#$%&(){}*+,/:;<>?\\\[\]] |
<text,braced>\\[0-9]		|
<text,braced>\\[a-zA-Z]+	|
<text,braced>.			{addvalue(yytext);}
%{
	/******* the end of the record ********/
%}
<comma,token,quoted>{end}	{ if (YY_START!=comma)
				     {UNNEST; dosetfield(); }
				  match_block();
				  docloserecord();
				  BEGIN INITIAL; }
<body>{end}			{ /*biberr(1,"trailing comma in record","");*/
				  match_block();
				  docloserecord();
				  BEGIN INITIAL; }
<body>{end}/{extrafield}	{ biberr(3,"record terminator ignored",""); }
<equals,value>{end}		{ biberr(3,"missing value for field",keyword);
				 dosetfield(); 
				 match_block();
				  docloserecord();
                                  BEGIN INITIAL;}
<body,equals,token,quoted,text,value,braced,preamble><<EOF>>	{
				  curtail();
				  biberr(3,"end of file within record","");
				  yyterminate(); }
<INITIAL><<EOF>> 		{ yyterminate(); }
<<EOF>>				{ biberr(1,"missing newline at end of file","");
				  yyterminate(); }
%{
	/****** this is a serious error ******/
%}
.				{ biberr(3,"DIDN\'T PARSE CHARACTER",yytext); }
%%

/* recognise record type */
recognise_type()
{ copylower(keyword,yytext);
  if (strcmp(keyword,"bibtex-file") ==0 ||
      strcmp(keyword,"bibtexfile")  ==0 ||
      strcmp(keyword,"bibliography")==0)  {code |= IS_HEADER|IS_STRUCTURED; }
  else if (strcmp(keyword,"string")==0)   {code |= IS_STRINGS; }
  else if (strcmp(keyword,"preamble")==0) {code |= IS_PREAMBLE|NO_LABEL; }
  else if (is_structured_comment(keyword)){code |= IS_STRUCTURED;}
  else if (strcmp(keyword,"comment")==0)  {code |= IS_COMMENT|NO_LABEL; }
  else {code |= WANT_LABEL; }

  switch ( code & (IS_STRUCTURED | HAVE_AT | HAVE_PCENT)) {
  case IS_STRUCTURED | HAVE_AT | HAVE_PCENT:
	biberr(2,"don't use both % and @ with",keyword);
	BEGIN pcopen;
	break;
  case IS_STRUCTURED | HAVE_AT:
	biberr(1,"use % not @ with",keyword);
	BEGIN open;
	break;
  case IS_STRUCTURED | HAVE_PCENT:
	/* structured comment with % is fine */
	BEGIN pcopen;
	break;
  case IS_STRUCTURED:
	biberr(1,"% inserted before",keyword);
	BEGIN open;
	break;
  case HAVE_AT | HAVE_PCENT:
	biberr(1,"% ignored before @",keyword);
	BEGIN pcopen;
	break;
  case HAVE_PCENT:
	/* ordinary record with % is ignored */
	BEGIN INITIAL;
	break;
  case HAVE_AT:
	/* ordinary record with @ is fine */
	BEGIN open;
	break;
  case 0:
	biberr(2,"@ inserted before",keyword);
	BEGIN open;
  }
if (code&NO_LABEL) {BEGIN nolabel;}
}

doopenrecord(thelabel)
char *thelabel;
{
    if ( savedcomments[0]!='\0')
	{
	    bibcomment(savedcomments); 
	    savedcomments[0]='\0';
	}
    inrecord=1;
    openrecord(keyword,thelabel,code,begin_at_line);
}

docloserecord()
{
    commentfield();
    closerecord(input_line_number);
    inrecord=0;
}

/* an @ or EOF has occurred unexpectedly: close the record */
curtail()
{ switch (YY_START) {
  case bracecomment:
	while (YY_START==bracecomment) {UNNEST; savecomment("}");}
	/* drop through */
  case comment:
	UNNEST; savecomment("}");
  case pcent:
	break;
  case braced:
	while (YY_START==braced) {UNNEST; strcat(thevalue,"}");}
	/* drop through */
  case text:
  case quoted:
  case token:
	UNNEST;
	/* drop through */
  case value:
  case equals:
	dosetfield();
	/* drop through */
  case body:
	closerecord(input_line_number);
	break;
}}

match_block()
{
	char msg[40];
	if (record_opener=='(' && yytext[0]==')' ||
	    record_opener=='{' && yytext[0]=='}') return;
	sprintf(msg,"record opened by %c closed by %c",
		record_opener,yytext[0]);
	return;		
}

resetvalue()
{
   valuebuffer[0]='\0'; nconcats=0; thevalue=valuebuffer;
    concat[nconcats]=thevalue;
    concattype[nconcats]=CMPT_IS_STR;
}

concatop()
{
    int cmptlen=strlen(thevalue);
    if (cmptlen==0) { return; }
    if (concattype[nconcats]==CMPT_IS_STR)
	{ int i;
	  char ch;
	  concattype[nconcats]=CMPT_IS_NUM;
	  for (i=0;i<cmptlen;i++) {
	    ch=thevalue[i];
	    if ((ch<'0') || ('9'<ch))
		{ concattype[nconcats]=CMPT_IS_STR; break; }
	  }
	}
    thevalue+=cmptlen+1;
    thevalue[0]='\0';
    nconcats++;
    concat[nconcats]=thevalue;
    concattype[nconcats]=CMPT_IS_STR;
}

dosetfield()
{
    char *thetext=concat[0];
    concatop();
    commentfield();
    if (nconcats==1 && thetext[0]=='{' ) {
      /* remove extra well-matched braces from around the text */
      int l=strlen(thetext);
      int nbra=0;
      int nket=0;
      int firstnonbra, lastnonket, level, i, minlevel;
      for (firstnonbra=0; firstnonbra<l; firstnonbra++) {
	char ch=thetext[firstnonbra];
	if (ch=='{') {nbra++;}
	else if (ch!=' ' && ch!='\t' && ch!='\n') {break;}}
      for (lastnonket=l-1; lastnonket>=0; lastnonket--) {
	char ch=thetext[lastnonket];
	if (ch=='}') {nket++;}
	else if (ch!=' ' && ch!='\t' && ch!='\n') {break;}}
      level=nbra;
      minlevel=level;
      if (minlevel>nket) {minlevel=nket;}
      for (i=firstnonbra; i<=lastnonket; i++) {
	char ch=thetext[i];
	if (ch=='{') {level++;}
	if (ch=='}') {level--;}
	if (level<minlevel) {minlevel=level;}
      }
      if (minlevel>0) {
	  /* printf("firstnonbra=%d, minlevel=%d, lastnonket=%d in %s",
	       firstnonbra, minlevel, lastnonket, thetext); */
	biberr(1,"extra braces ignored","");
      nbra=0;
      nket=0;
      for (firstnonbra=0; firstnonbra<l && nbra<minlevel; ) {
	char ch=thetext[firstnonbra++];
	if (ch=='{') {nbra++;}
      }
      for (lastnonket=l; lastnonket>=0 && nket<minlevel; ) {
	char ch=thetext[--lastnonket];
	if (ch=='}') {nket++;}
      }
      thetext[lastnonket]='\0';
      concat[0]=&thetext[firstnonbra];
      }}
    /* output the value */
    if (code&IS_STRINGS)
     { setmacro(keyword,nconcats,concat,concattype,code,input_line_number);}
    else if (code&IS_PREAMBLE)
	{ setpreamble(nconcats,concat,concattype,code,input_line_number); }
    else { setfield(keyword,nconcats,concat,concattype); }
    resetvalue();
}

/* append the text just read to the pending field value,
   adding intermediate space if necessary */
addvalue(addition)
char *addition;
{
        int j=strlen(thevalue);
	int new_separator=0;
	int len=strlen(addition);
	if (len==0) return;
        if (separator!=0 && j!=0)
            { switch (separator) {
	    case 1:
		thevalue[j++]=' ';
		break;
	    case 3:
		thevalue[j++]='\n';
		/* drop through to add another one */
	    case 2:
		thevalue[j++]='\n';
		break;
	    }
	    thevalue[j]='\0';}
	strncpy (&thevalue[j],addition,len);
	thevalue[j+len]='\0';
	separator=0;
	return;
}

addmacro(addition)
char *addition;
{
    concatop();
    concattype[nconcats]=CMPT_IS_MACRO;
    strcpy(thevalue,addition);
    concatop();
}

/* copy src to dest, translating capitals to lower case */
copylower(dest,src)
char *dest;
char *src;
{
    int i;
    char ch;
    for (i=0;(ch=src[i])!='\0';i++)
        { if (ch>='A' && ch<='Z') { ch+='a'-'A'; }
	  dest[i]=ch;
	}
    dest[i]='\0';
}


commentfield()
{
 /* embedded in record: generate a "comment" field */
    char *data[1];
    int datatype[1];
    int len;
    /* forget it if empty, or in @string @preamble or @comment */
    if ( savedcomments[0]=='\0') {return;}
    if ( code & (IS_STRINGS | IS_PREAMBLE | IS_COMMENT) ) { return;}
    /* trim trailing newline */
    len=strlen(savedcomments)-1;
    if (savedcomments[len]=='\n')
	{ savedcomments[len]=='\0';
	  if (len==0) { return;}
	}
    /* construct the vector */
    data[0]=savedcomments;
    datatype[0]=CMPT_IS_STR;
    setfield("comment",1,data,datatype);
    savedcomments[0]='\0';
}

inc_line_no()
{
    int i;
    for (i=0;i<yyleng;i++)
	{ if (yytext[i]=='\n')
	    { if ( savedcomments[0]!='\0')
		    { if ( inrecord )
			{/* save for comment field in current record */
			    int len=strlen(savedcomments);
			    if (len>0 && savedcomments[len-1]!='\n')
				{
				    savedcomments[len++]='\n';
				    savedcomments[len]='\0';
				}
			}
		    else
			{/* outside record: output comments */
			    bibcomment(savedcomments); 
			    savedcomments[0]='\0';
			}
		    }
		input_line_number++;
	    }
	}
}


numaccent(ch,msg)
int ch;
char *msg;
{
char addition[20];
char message[50];
int embrace=(YY_START==text)?1:0;

static char *latin1[96]={
 /* from "A0 ('200, 160) to "BF ('277, 191) */
 /*  048C         159D            26AE                    37BF */
 "~",              "!\'",          "\\textcent",           "\\pounds",
 "\\textcurrency", "\\textyen",    "\\textbrokenbar",      "\\S",
 "\\\"{}",         "\\copyright",  "\\masculine",          "\\guillemotleft",
 "\\lnot",         "--",           "\\textregistered",     "\\={}",
 "\\mathdegree",   "\\pm",         "\\mathtwosuperior",  "\\maththreesuperior",
 "\\\'{}",         "\\mu",         "\\P",                  "\\cdot",
 "\\c{}",     "\\mathonesuperior", "\\mathordfeminine",    "\\guillemotright",
 "\\textonequarter", "\\textonehalf", "\\textthreequarters", "?\'",
 /* from "C0 ('300, 192) to "FF ('377, 255) */
 /* 0/8      1/9      2/A      3/B      4/C       5/D      6/E      7/F */
 "\\`A", "\\\'A", "\\^A",  "\\~A",  "\\\"A",  "\\AA",    "\\AE",   "\\c{C}",
 "\\`E", "\\\'E", "\\^E",  "\\\"E", "\\`I",   "\\\'I",   "\\^I",   "\\\"I",
 "\\DH", "\\~N",  "\\`O",  "\\\'O", "\\^O",   "\\~O",    "\\\"O",  "\\times",
 "\\O",  "\\`U",  "\\\'U", "\\^U",  "\\\"U",  "\\\'Y",   "\\TH",   "\\ss",
 "\\`a", "\\\'a", "\\^a",  "\\~a",  "\\\"a",  "\\aa",    "\\ae",   "\\c{c}",
 "\\`e", "\\\'e", "\\^e",  "\\\"e", "\\`\\i", "\\\'\\i", "\\^\\i", "\\\"\\i",
 "\\dh", "\\~n",  "\\`o",  "\\\'o", "\\^o",   "\\~o",    "\\\"o",  "\\div",
 "\\o",  "\\`u",  "\\\'u", "\\^u",  "\\\"u",  "\\\'y",   "\\th",   "\\\"y"};

ch &= 255;
if (ch<162||ch==191) {embrace=0;}
if (ch>=160 && ch <256) {
	sprintf(addition,(embrace?"{%s}":"%s"),latin1[ch-160]);
	addvalue(addition);
	sprintf(message,"translated %s into %s",msg,addition); }
else {	sprintf(message,"%s %d unrecognised",msg,ch); }

biberr(1,message,"");
return;
}

htmlaccent(accent,letter)
char *accent;
char letter;
{
char addition[20];
sprintf(addition,((YY_START==text)?"{%s%c}":"%s%c"),accent,letter);
addvalue(addition);
biberr(1,"translated HTML accent into",addition);
}

htmlsym(accent)
char *accent;
{
char addition[20];
sprintf(addition,((YY_START==text)?"{%s}":"%s"),accent);
addvalue(addition);
biberr(1,"translated HTML symbol into",addition);
}

savecomment(addition)
char *addition;
{
    strcat(savedcomments,addition);
}

int is_structured_comment(string)
char *string;
{
	if (strcmp(string,"bibtex-file")==0) {return 1;}
	if (strcmp(string,"bibtexfile")==0) {return 1;}
	if (strcmp(string,"bibliography")==0) {return 1;}
	return 0;
}
