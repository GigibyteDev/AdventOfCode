// For more information see https://aka.ms/fsharp-console-apps
open Helper

module Cards =
    type Label =
    | Ace
    | King
    | Queen
    | Jack
    | Ten
    | Nine
    | Eight
    | Seven
    | Six
    | Five
    | Four
    | Three
    | Two
    with
        static member LabelValues = [|Two,1;Three,2;Four,3;Five,4;Six,5;Seven,6;Eight,7;Nine,8;Ten,9;Jack,10;Queen,11;King,12;Ace,13|] |> Map.ofArray
        static member private ConversionValues = [|"2",Two;"3",Three;"4",Four;"5",Five;"6",Six;"7",Seven;"8",Eight;"9",Nine;"T",Ten;"J",Jack;"Q",Queen;"K",King;"A",Ace|] |> Map.ofArray
        static member ofChar char = Label.ConversionValues |> Map.tryFind char |> Option.defaultValue Two
        static member beats curr other =
            Label.LabelValues[curr] > Label.LabelValues[other]
        static member beatsJoker curr other =
            match curr, other with
            | Jack, _ ->
                false
            | _, Jack ->
                true
            | _ ->
                Label.beats curr other

    type HandPower =
    | FiveOfAKind
    | FourOfAKind
    | FullHouse
    | ThreeOfAKind
    | TwoPair
    | OnePair
    | HighCard
    with
        static member LabelValues = [|HighCard,1;OnePair,2;TwoPair,3;ThreeOfAKind,4;FullHouse,5;FourOfAKind,6;FiveOfAKind,7|] |> Map.ofArray
        member this.beats other =
            HandPower.LabelValues[this] > HandPower.LabelValues[other]
    
    let rec retrieveCardsToMatch (p1Cards: Label array) p2Cards iter =
        if (iter >= (p1Cards |> Array.length) || iter >= (p2Cards |> Array.length)) then
            p1Cards[0],p2Cards[0]
        else
            match p1Cards[iter] = p2Cards[iter] with
            | true ->
                retrieveCardsToMatch p1Cards p2Cards (iter + 1)
            | false ->
                p1Cards[iter],p2Cards[iter]

    type Hand = {
        cards: Label array
        bid: int
    }
    with
        static member init line =
            let handAndBid = line |> String.splitOnString " "
            let (hand, bid) = handAndBid |> Array.head, handAndBid |> Array.last
            {
                cards = hand |> Seq.map(fun (c:char) -> Label.ofChar (string c)) |> Array.ofSeq
                bid = bid |> System.Int32.Parse
            }
        static member getGrouping (this: Hand) =
            this.cards
            |> Array.groupBy id
            |> Array.map(fun (key, v) -> key, v |> Array.length)
            |> Map.ofArray

        static member getGroupingWithJokers (this: Hand) =
            match this.cards |> Array.exists(fun c -> c = Jack) && this.cards |> Seq.exists(fun c -> c = Jack |> not) with
            | true ->
                let nonJokers = this.cards |> Array.filter(fun c -> c = Jack |> not)
                let nonJokerGrouping =
                    nonJokers
                    |> Seq.groupBy id
                    |> Seq.map(fun (key, v) -> key, v |> Seq.length)
                    |> Map.ofSeq
                let largestCard =
                    nonJokerGrouping
                    |> Map.fold(fun labelToAdd key v ->
                        match v > nonJokerGrouping[labelToAdd] with
                        | true -> key
                        | false -> labelToAdd
                    ) (nonJokers |> Seq.head)
                nonJokerGrouping.Add(largestCard, nonJokerGrouping[largestCard] + ((this.cards |> Seq.length) - (nonJokers |> Seq.length)))
            | false ->
                Hand.getGrouping this

        static member getHandFromGrouping grouping =
            match grouping |> Seq.length with
            | 1 ->
                FiveOfAKind
            | 2 ->
                match grouping |> Map.values |> Seq.contains 4 with
                | true -> FourOfAKind
                | false -> FullHouse
            | 3 ->
                match grouping |> Map.values |> Seq.contains 3 with
                | true ->
                    ThreeOfAKind
                | false ->
                    TwoPair
            | 4 ->
                OnePair
            | _ ->
                HighCard

        static member power groupingLogic this =
            this |> groupingLogic |> Hand.getHandFromGrouping

        static member beats this (other:Hand) labelCompare groupingLogic =
            let thisHandPower = (this |> Hand.power groupingLogic)
            let otherHandPower = (other |> Hand.power groupingLogic)
            match thisHandPower = otherHandPower with
            | true ->
                let (p1Card, p2Card) = retrieveCardsToMatch this.cards other.cards 0
                match p1Card = p2Card with
                | true ->
                    0
                | false ->
                    match labelCompare p1Card p2Card with | true -> 1 | false -> -1
            | false ->
                match thisHandPower.beats otherHandPower with | true -> 1 | false -> -1    
                
    let compileLines labelCompare groupingLogic lines =
        lines
        |> Seq.map(fun line -> (line |> Hand.init))
        |> Array.ofSeq
        |> Array.sortWith(fun curr other -> Hand.beats curr other labelCompare groupingLogic)
        |> Array.mapi(fun i h -> 
            (i + 1) * h.bid
        )
        |> Array.sum

module Part1 =
    open Cards
    let total =
        compileLines Label.beats Hand.getGrouping

module Part2 =
    open Cards
    let total =
        compileLines Label.beatsJoker Hand.getGroupingWithJokers
        
input
|> outputFileResult Part1.total "Part 1"

input
|> outputFileResult Part2.total "Part 2"
