#load "Bridge.fs"
open Bridge
let check message condition = if not condition then failwith message
let make level strain multiplier = { Level = level; Strain = strain; Declarer = 2; Multiplier = multiplier }
check "4H non-vulnerable game" (score 1 (make 4 2 1) 10 = 420)
check "4H vulnerable game" (score 2 (make 4 2 1) 10 = 620)
check "3NT with overtrick" (score 1 (make 3 4 1) 10 = 430)
check "6NT vulnerable slam" (score 2 (make 6 4 1) 12 = 1440)
check "Doubled down four" (score 1 (make 4 2 2) 6 = -800)
check "Redoubled making with overtrick" (score 1 (make 2 2 4) 9 = 840)
let auction = [0, Pass; 3, Pass; 2, Pass; 1, Redouble; 0, Double; 3, Bid (4, 3); 2, Pass; 1, Bid (1, 3)]
check "First bidder declares redoubled contract" (contract auction = Some { Level = 4; Strain = 3; Declarer = 1; Multiplier = 4 })
let mutable played = 0
for board in 1..500 do
    let hands = deal ()
    check "Deal size or duplicate" (Array.forall (fun h -> List.length h = 13) hands && (hands |> Seq.concat |> Set.ofSeq |> Set.count) = 52)
    let mutable history = []
    let mutable seat = (board - 1) % 4
    while not (auctionOver history) do
        let call = acol history seat hands[seat]
        check "Illegal bot call" (legal history seat call)
        history <- (seat, call) :: history
        seat <- next seat
        check "Auction did not terminate" (List.length history <= 320)
    match contract history with
    | None -> ()
    | Some c ->
        played <- played + 1
        seat <- next c.Declarer
        for _ in 1..13 do
            let mutable trick = []
            for _ in 1..4 do
                let card = botCard c.Strain seat trick hands[seat]
                check "Illegal bot card" (legalCards trick hands[seat] |> List.contains card)
                hands[seat] <- List.filter ((<>) card) hands[seat]
                trick <- trick @ [seat, card]
                seat <- next seat
            seat <- winner c.Strain trick
        check "Cards remain" (Array.forall List.isEmpty hands)

// Bidding and card-play regressions. Hands are spades, hearts, diamonds, clubs;
// the seats not given get the rest of the deck, dealt alternately.
let rankOf ch = match ch with 'A' -> 14 | 'K' -> 13 | 'Q' -> 12 | 'J' -> 11 | 'T' -> 10 | c -> int c - int '0'
let hand (s: string) (h: string) (d: string) (c: string) =
    [ for suit, cards in [3, s; 2, h; 1, d; 0, c] do for ch in cards -> { Suit = suit; Rank = rankOf ch } ]
let complete given =
    let used = given |> List.collect snd |> Set.ofList
    let rest = [ for s in 0..3 do for r in 2..14 -> { Suit = s; Rank = r } ] |> List.filter (used.Contains >> not)
    let others = [0..3] |> List.filter (fun s -> not (List.exists (fst >> (=) s) given))
    let hs = Array.create 4 []
    for s, h in given do hs[s] <- h
    rest |> List.iteri (fun i c -> let s = others[i % others.Length] in hs[s] <- c :: hs[s])
    check "Test deal" (Array.forall (fun h -> List.length h = 13) hs && Set.count used = List.sumBy (snd >> List.length) given)
    hs
let bidOut dealer (hs: Card list array) =
    let mutable h = []
    let mutable seat = dealer
    while not (auctionOver h) do
        h <- (seat, acol h seat hs[seat]) :: h
        seat <- next seat
    h
let callsBy seat h = h |> List.rev |> List.choose (fun (s, c) -> if s = seat then Some c else None)
let reached h level strain declarer =
    contract h |> Option.exists (fun c -> (c.Level, c.Strain, c.Declarer) = (level, strain, declarer))
let strong = hand "AKQ2" "AKQ" "AQ3" "432"
check "2C opener bids game over a 3C positive"
    (reached (bidOut 0 (complete [0, strong; 2, hand "543" "543" "K2" "AKJ65"])) 3 4 0)
check "Diamond positive to 2C is 3D, not the 2D negative"
    (callsBy 2 (bidOut 0 (complete [0, strong; 2, hand "J43" "543" "KJT65" "K7"])) |> List.head = Bid (3, 1))
check "20-count opener accepts a 3H limit raise"
    (reached (bidOut 0 (complete [0, hand "AK32" "AKJ54" "K2" "Q2"; 2, hand "54" "Q876" "A543" "KJ3"])) 4 2 0)
check "Natural 2C overcall is not answered as a strong 2C"
    (reached (bidOut 1 (complete [1, hand "AKJ543" "K32" "Q2" "32"; 2, hand "T7" "A54" "K43" "AKJ54"
                                  0, hand "Q98" "QJ9" "8765" "Q76"])) 2 0 2)
check "4-4-4-1 opener does not pass a 1S response"
    (callsBy 0 (bidOut 0 (complete [0, hand "2" "AQ54" "KJ54" "A543"; 2, hand "KQ54" "632" "Q32" "Q72"])) |> List.item 1 <> Pass)
let spadeAce = { Suit = 3; Rank = 14 }
let voidHand = [ { Suit = 2; Rank = 9 }; { Suit = 2; Rank = 2 }; { Suit = 0; Rank = 3 } ]
check "Void hand discards rather than trumping partner's winner" (botCard 2 1 [3, spadeAce] voidHand = { Suit = 0; Rank = 3 })
check "Void hand ruffs an opponent's winner with its lowest trump" (botCard 2 1 [0, spadeAce] voidHand = { Suit = 2; Rank = 2 })
printfn "Verified 500 auctions, %d full deals, scoring examples, and bidding and play regressions." played
