-module(huffman).
-compile(export_all).

sample() ->
    "the quick brown fox jumps over the lazy dog this is a sample text that we will use when we build up a table we will only handle lower case letters and no punctuation symbols the frequency will of course not represent english but it is probably not that far off".

text() ->
    "tttccdefghhhhh".


test() ->
    encode(text(), encode_table(tree(text()))).

% test() ->
%     Sample = sample(),
%     Tree = tree(Sample).
    % Encode = encode_table(Tree),
    % Decode = decode_table(Tree),
    % Text = text(),
    % Seq = encode(Text, Encode),
    % Text = decode(Seq, Decode).

% %Creates the huffman tree from frequencies in the text
tree(Sample) ->
    Freq = freq(Sample),
    huffman(Freq).




%Find the frequency of the chars in the sample (external).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

freq(Sample) ->
    freq(Sample, []).

%Calculate frequency of all chars in list (internal)
freq([], Freq) ->
    Freq;
freq([Char|Rest], Freq) ->
    freq(Rest, freq_add(Char, Freq)).

%Adds one to the frequencycounter of the supplied char
freq_add(Char, []) ->
    [{Char, 1}];
freq_add(Char, [{Char, Occ} | Rest]) ->
    [{Char, Occ+1}|Rest];
freq_add(Char, [Head|Rest]) ->
    [Head|freq_add(Char, Rest)].






%Cunstructs the huffman tree from a list of frequencies
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

huffman(Freq) ->
    huffman_construct_tree(huffman_order(Freq, [])).

%Construct the huffman tree from a sorted list of leafs
%Adds the two first elements together and then their values. This is inserted at the correct position in the tree
huffman_construct_tree([E1 = {_, V1}, E2 = {_, V2}]) ->
    {{E1, E2}, V1+V2};
%This makes the single element appear last in the list (not wanted?)
% huffman_construct_tree([{_, V1} = H1,{_, V2} =  H2, T]) ->
%     [{{H1, H2}, V1 + V2}, T];
huffman_construct_tree([{_, V1} = H1,{_, V2} =  H2 | T]) ->
    huffman_construct_tree(huffman_insert({{H1, H2}, V1 + V2}, T)).

%Sort the leafs.
huffman_order([], Freq_sorted) ->
    Freq_sorted;
huffman_order([Freq_h|Freq_t], Freq_sorted) ->
    huffman_order(Freq_t, huffman_insert(Freq_h, Freq_sorted)).


%Insert a leaf at the correct location.
huffman_insert(Leaf, []) ->
    [Leaf];
huffman_insert({_, L_Val} = Leaf, [{_, E_Val} = H | T]) when L_Val > E_Val->
    [H | huffman_insert(Leaf, T)];
huffman_insert(Leaf, Trailing) ->
    [Leaf | Trailing].




%Creates the encoded huffman table
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%If there are any sub-branches left
encode_table(Table) ->
    encode_table(Table, []).

encode_table({{Branch1, Branch2}, _}, List) ->
    encode_table(Branch1, [0 | List]) ++ encode_table(Branch2, [1 | List]);
%Else : we have reached the bottom of the tree.
encode_table({C, _}, List) ->
    [{C, List}].





%Encode the table as a list of 1's and 0's
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Only one char left!
encode([Char], Table) ->
    encode_char(Char, Table);

%Add the first encoded char to the rest of the string (encoded)
encode([Char|Text_left], Table) ->
    encode_char(Char, Table) ++ encode(Text_left, Table).

%Encode a char using the encoded huffman table
encode_char(Char, [{Data_char, Data} | Table_left]) ->
    if
        Char == Data_char ->
            Data;
        true ->
            encode_char(Char, Table_left)
    end.
