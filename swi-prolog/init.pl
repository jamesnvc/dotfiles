/*  -*- Prolog -*-

    SWI-Prolog personalization file
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This is a sample user-initialisation file for SWI-Prolog. If you wish to
customise prolog, make  a  copy  of  this   file  and  edit  it  to your
preferences.

Installation:

	Unix/MacOS:	~/.swiplrc
	Windows:	<appdata>/swipl.ini (see win_folder(appdata, AppData))

More hints on useful things you  can  put   into  this  file  are in the
SWI-Prolog reference manual. Notably look   at debugger settings, editor
hooks, file_search_path/2, set_prolog_flag/2 and portray/1.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

		 /*******************************
		 *	       IDE		*
		 *******************************/

%	By default, xpce (the  graphics  library)   runs  in  a seperate
%	thread. This allows editing and inspecting your program while it
%	is running. All components of the Prolog development environment
%	are aware of this. If you use   your own xpce based applications
%	and if you develop using xpce, it  might be wise to disable this
%	feature and keep xpce in the thread `main`.

% :- set_prolog_flag(xpce_threaded, false).


		 /*******************************
		 *	      EDITOR		*
		 *******************************/

%	Define the editor to use.  Note that more advanced manipulation
%	of this is defined in the SWI-Prolog reference manual, section
%	"Listing and Editor Interface"
%
%	The value pce_emacs (or built_in) causes the system to use the
%	built-in editor PceEmacs if the environment provides for a GUI.
%	pce_emacs is the default if XPCE is available.
%
%	The second entry defines an arbitrary editor and how to tell
%	SWI-Prolog to open a file with it on a specified line-number.

% :- set_prolog_flag(editor, pce_emacs).
% :- set_prolog_flag(editor, pico).
%% :- set_prolog_flag(editor, emacs).

%:- multifile
%	prolog_edit:edit_command/2.
%
prolog_edit:edit_command('emacsclient --create-frame', '%e +%d "%f"').
%prolog_edit:edit_command(pico, '%e "%f"').


		 /*******************************
		 *	      DEBUGGING		*
		 *******************************/

%	If you prefer graphical tracing, add the line below.

% :- (current_prolog_flag(gui, true) -> guitracer ; true).

%	Determine how terms are printed by the debugger and toplevel.  The
%	values here are defaults. max_depth(10) replaces all subterms at
%	a greater depth with elipses (...). See write_term/3 for further
%	explanation and more options.

% :- set_prolog_flag(answer_write_options,
%		     [quoted(true), portray(true), max_depth(10)]).
% :- set_prolog_flag(debugger_write_options,
%		     [quoted(true), portray(true), max_depth(10)]).

%	If you want to suppress printing toplevel query variables
%	starting with an `_'

% :- set_prolog_flag(toplevel_print_anon, false).

%	If you do not want the tracer to stop at at the exit port.

% :- leash(-exit).

%	Uncomment the two lines below  to   include  a  backtrace of the
%	stack on an uncaught exception.  This   makes  the system rather
%	noisy on exceptions, but often allows   you to discover why your
%	program misbehaves without using the debugger.

% :- load_files(library(prolog_stack)).
% prolog_stack:stack_guard(none).

		 /*******************************
		 *	 CONSOLE FEEDBACK	*
		 *******************************/

%	Use the flag below to  disable   coloured  output  in all cases.
%	Normally,  coloured  output  is  enabled  if  the  output  is  a
%	terminal.

% :- set_prolog_flag(color_term, false).

%	Specify colors for the above,  based   on  the  message kind See
%	ansi_format/3 for specifying visual  effects.   The  table below
%	duplicates the default behavior.  Notably   on  terminals with a
%	dark background, yellow might be a   better  choice for warnings
%	and errors.

%:- multifile user:message_property/2.
%
%user:message_property(informational, color(fg(green))).
%user:message_property(information,   color(fg(green))).
%user:message_property(debug,         color(fg(blue))).
%user:message_property(warning,       color(fg(red))).
%user:message_property(error,         color([fg(red),bold])).

%	Specify feedback for loading files.  Values are `full` (feedback
%	at start and end of each  file),   `normal`  (feedback at end of
%	each file), `brief` (feedback  at  end   of  toplevel  file) and
%	`silent` (no feedback).

% :- set_prolog_flag(verbose_load, silent).


		 /*******************************
		 *     COMMAND LINE HISTORY	*
		 *******************************/

%	If you want to access the command-history like a Unix shell,
%	set =history= to the number of commands to remember.

% :- set_prolog_flag(history, 50).

% Set =save_history= to =false= if you never want to save/restore the
% command history.   Normally, the history is enabled if the system
% provides a history and the input comes from a terminal.

%% :- initialization
%%    new(_, font(courier,roman, 14, 'PragmataPro Mono:size=14')).

%:- set_prolog_flag(autoload, false).

% helper function to load tests
load_tests_dir :-
    working_directory(Cwd, Cwd),
    absolute_file_name("../test", TestDir, [relative_to(Cwd),
                                            file_type(directory)]),

    % library(filesex)
    findall(File,
            directory_member(TestDir, File, [recursive(true),
                                             extensions(['plt'])]),
            TestFiles),
    load_files(TestFiles, [if(changed), imports([])]).


print_chpoint :-
   prolog_current_choice(ChI),
   prolog_choice_attribute(ChI,frame,F),
   prolog_frame_attribute(F,goal,Goal),
   format('Last choice point: ~w', [ Goal ]).
