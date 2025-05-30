% алфавит
alphabet([a,b,c,d,e,f,g]).

% точные нужные количества
target_counts([a-4, b-3, c-3]).

% генерировать все слова длины N, удовлетворяющие условиям
generate_words(N, FileName) :-
    open(FileName, write, Stream),
    word_of_length(N, Word),
    valid_word(Word),
    atom_chars(Atom, Word),
    writeln(Stream, Atom),
    fail.
generate_words(_, FileName) :-
    close(FileName).

% генерация слова длины N из алфавита
word_of_length(0, []) :- !.
word_of_length(N, [L|Rest]) :-
    N > 0,
    alphabet(Alpha),
    member(L, Alpha),
    N1 is N - 1,
    word_of_length(N1, Rest).

% проверка, что слово удовлетворяет условиям
valid_word(Word) :-
    count_letters(Word, Counts),
    target_counts(Targets),
    % обязательные количества (a-4, b-3, c-3)
    member(a-Ac, Counts), Ac =:= 4,
    member(b-Bc, Counts), Bc =:= 3,
    member(c-Cc, Counts), Cc =:= 3,
    % остальные буквы максимум один раз
    check_optional(d, Counts),
    check_optional(e, Counts),
    check_optional(f, Counts),
    check_optional(g, Counts),
    % общая длина совпадает
    sum_counts(Counts, Total),
    length(Word, Total).

% Подсчет количества каждой буквы
count_letters(Word, Counts) :-
    alphabet(Alpha),
    count_each(Alpha, Word, Counts).

count_each([], _, []).
count_each([L|Ls], Word, [L-C|Rest]) :-
    count(L, Word, C),
    count_each(Ls, Word, Rest).

count(_, [], 0).
count(X, [X|T], N) :-
    count(X, T, N1),
    N is N1 + 1.
count(X, [Y|T], N) :-
    X \= Y,
    count(X, T, N).

% проверка, что буква встречается не более одного раза
check_optional(Letter, Counts) :-
    ( member(Letter-C, Counts) -> C =< 1 ; true ).

% сумма всех количеств
sum_counts([], 0).
sum_counts([_-C|T], Sum) :-
    sum_counts(T, Rest),
    Sum is C + Rest.
