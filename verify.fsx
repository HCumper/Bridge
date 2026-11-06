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
printfn "Verified 500 auctions, %d full deals, and scoring examples." played
