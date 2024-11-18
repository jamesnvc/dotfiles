:- module(web2cbz, []).

:- use_module(library(http/http_open)).
:- use_module(library(http/http_client)).
:- use_module(library(sgml), [load_html/3]).
:- use_module(library(xpath)).
:- use_module(library(archive), [archive_create/3]).
:- use_module(library(optparse)).

/*
find_image_url(DOM, ImageLink) :-
    once(xpath(DOM, //div(@id=comic)/a/img(@src), ImageLink)).
find_image_url(DOM, ImageLink) :-
    once(xpath(DOM,
               //article/div(@class='post-content')/div(@class=entry)/p/img(@'data-orig-file'),
               ImageLink)).

find_next_url(DOM, NextLink) :-
    once(xpath(DOM, //a(@class='comic-nav-base comic-nav-next', @href), NextLink)).
*/

:- dynamic find_image_url/2.
:- dynamic find_next_url/2.

test_xpath_query(URL) :-
    load_html(URL, DOM, []),
    debug(web2cbz, "Got DOM for ~w", [URL]),
    ( find_image_url(DOM, ImageLink) ->
      debug(web2cbz, "  Got image link ~w", [ImageLink])
    ; debug(web2cbz, "  Couldn't find image link", []) ),
    ( find_next_url(DOM, NextLink) ->
      debug(web2cbz, "  Got next link ~w", [NextLink])
    ; debug(web2cbz, "  Couldn't find next link", [])).

process_page(Counter, URL) :-
    load_html(URL, DOM, []),
    debug(web2cbz, "Got DOM for ~w", [URL]),
    find_image_url(DOM, ImageLink),
    debug(web2cbz, "  Got image link ~w", [ImageLink]),
    format(string(ImageFileName), "images/~|~`0t~d~3+.jpg", [Counter]),
    setup_call_cleanup(open(ImageFileName, write, OutStream, [type(binary)]),
                       http_get(ImageLink, _, [to(stream(OutStream))]),
                       close(OutStream)
                      ),
    debug(web2cbz, "   Saved image link to ~w", [ImageFileName]),
    ( find_next_url(DOM, NextLink)
    -> ( Counter1 is Counter + 1,
         sleep(1),
         process_page(Counter1, NextLink))
    ; true ).

directory_cbz(Name, Directory) :-
    format(string(FileName), "~w.cbz", [Name]),
    directory_files(Directory, Files0),
    findall(File,
            ( member(File, Files0),
              file_name_extension(_, jpg, File) ),
            Files1),
    sort(Files1, Files),
    length(Files, NumFiles),
    debug(web2cbz, "Creating CBZ with ~w files...", [NumFiles]),
    archive_create(FileName, Files, [format(zip), directory(Directory)]),
    debug(web2cbz, "Created archive ~w", [FileName]).

% web2cbz:process_page(0, 'https://swanboy.com/comic/swan-boy-joins-the-gym/').
% web2cbz:process_page(28, 'https://swanboy.com/comic/208/').
% done at 317, https://swanboy.com/comic/capitalonebankguy6/

:- initialization(main, main).

ensure_list(X, X) :- is_list(X), !.
ensure_list(X, [X]).

parse_xpath_selector(Atom, Sels) :-
    read_term_from_atom(Atom, SelOrSels, [ module(web2cbz) ]),
    ensure_list(SelOrSels, Sels).

main(Args) :-
    OptsSpec = [
        [ opt(starturl), type(atom), shortflags([u]),
          longflags(['start-url']),
          help("URL from which to begin downloading images and following links") ],

        [ opt(nextlink), type(atom), shortflags([n]),
          longflags([next]),
          help("XPath to locate the next link")
        ],

        [ opt(image), type(atom), shortflags([i]),
          longflags([image]),
          help("XPath to locate the image url")
        ],

        [ opt(quiet), type(boolean), shortflags([q]),
          longflags([quiet]), default(false),
          help("Turn off debug messages") ],

        [ opt(counterstart), type(number), default(0),
          shortflags([s]), longflags([counterstart]),
          help("Number to start image numbers at") ],

        [ opt(xpathtest), type(boolean), default(false),
          shortflags([x]), longflags([xpathtest])
        ],

        [ opt(help), type(boolean), default(false),
          shortflags([h]), longflags([help]) ]
    ],

    opt_parse(OptsSpec, Args, Opts, PosArgs),
    ( ( ground(Opts), PosArgs = [], memberchk(help(false), Opts) ) -> true ;
      ( opt_help(OptsSpec, HelpAtom),
        format(user_error, "~w", [HelpAtom]),
        halt(1)
      ) ),
    ( memberchk(quiet(false), Opts) -> debug(web2cbz) ; true ),

    memberchk(nextlink(LinkXpathAtom), Opts),
    nonvar(LinkXpathAtom),
    parse_xpath_selector(LinkXpathAtom, LinkXPaths),
    forall(member(XPath, LinkXPaths),
           assertz((find_image_url(DOM, NextLink) :-
                        xpath_chk(DOM, XPath, NextLink)))
          ),

    memberchk(image(ImageXpathAtom), Opts),
    nonvar(ImageXpathAtom),
    parse_xpath_selector(ImageXpathAtom, ImageXPaths),
    forall(member(XPath, ImageXPaths),
           assertz((find_next_url(DOM, ImageLink) :-
                        xpath_chk(DOM, XPath, ImageLink)
                   ))),

    compile_predicates([find_image_url/2, find_next_url/2]),

    memberchk(starturl(URL), Opts),
    memberchk(counterstart(Counter), Opts),
    ( memberchk(xpathtest(true), Opts) ->
      test_xpath_query(URL)
    ; process_page(Counter, URL)
    ).

% swipl -s web2cbz.pl -- --start-url 'https://swanboy.com/comic/laundry/' --next "[//a(@class='comic-nav-base comic-nav-next', @href), //article/div(@class='post-content')/div(@class=entry)/p/img(@'data-orig-file')]" --xpathtest --image '//div(@id=comic)/a/img(@src)'
