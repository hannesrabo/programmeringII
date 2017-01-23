-module(huffman).
-compile(export_all).

sample() ->
    "the quick brown fox jumps over the lazy dog this is a sample text that we will use when we build up a table we will only handle lower case letters and no punctuation symbols the frequency will of course not represent english but it is probably not that far off".

text() ->
    "tttccdefghhhhh".

% test() ->
%     Sample = sample(),
%     Tree = tree(Sample).
    % Encode = encode_table(Tree),
    % Decode = decode_table(Tree),
    % Text = text(),
    % Seq = encode(Text, Encode),
    % Text = decode(Seq, Decode).

% %Creates the huffman tree from frequencies in the text
% tree(Sample) ->
%     Freq = freq(Sample),
%     huffman(Freq).

%Find the frequency of the chars in the sample.
freq(Sample) ->
    freq(Sample, []).

freq([], Freq) ->
    Freq;
freq([Char|Rest], Freq) ->
    freq(Rest, freq_add(Char, Freq)).

freq_add(Char, []) ->
    [{Char, 1}];
freq_add(Char, [{Char, Occ} | Rest]) ->
    [{Char, Occ+1}|Rest];
freq_add(Char, [Head|Rest]) ->
    [Head|freq_add(Char, Rest)].


huffman(Freq) ->
    huffman_order(Freq, []).

huffman_order([], Freq_sorted) ->
    Freq_sorted;
huffman_order([Freq_h|Freq_t], Freq_sorted) ->
    huffman_order(Freq_t, huffman_insert(Freq_h, Freq_sorted)).

huffman_insert(Leaf, []) ->
    [Leaf];
huffman_insert({_, L_Val} = Leaf, [{_, E_Val} = H | T]) when L_Val < E_Val->
    [H | huffman_insert(Leaf, T)];
huffman_insert(Leaf, Trailing) ->
    [Leaf | Trailing].



% encode_table(_Tree) ->
%     .
%
% decode_table(_Tree) ->
%     .
%
% encode(_Text, _Table) ->
%     .
%
% decode(_Seq, _Tabl) ->
%     .
