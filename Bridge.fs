module Bridge

type Card = { Suit: int; Rank: int }
type Call = Pass | Bid of int * int | Double | Redouble
type Contract = { Level: int; Strain: int; Declarer: int; Multiplier: int }

let seats = [| "North"; "East"; "South"; "West" |]
let suits = [| "♣"; "♦"; "♥"; "♠"; "NT" |]
let partner seat = (seat + 2) % 4
let next seat = (seat + 1) % 4
let side seat = seat % 2
let callName = function
    | Pass -> "Pass" | Double -> "Double" | Redouble -> "Redouble"
    | Bid (level, strain) -> string level + suits[strain]
let cardName c =
    (match c.Rank with 14 -> "A" | 13 -> "K" | 12 -> "Q" | 11 -> "J" | n -> string n)
    + suits[c.Suit]
let deal () =
    let deck = [| for s in 0..3 do for r in 2..14 -> { Suit = s; Rank = r } |]
    System.Random.Shared.Shuffle deck
    Array.init 4 (fun i -> deck[i*13..i*13+12] |> Array.sortByDescending (fun c -> c.Suit, c.Rank) |> Array.toList)

// Auction history is newest first.
let lastBid history =
    history |> List.tryPick (function seat, Bid (l, s) -> Some (seat, l, s) | _ -> None)
let multiplier history =
    history |> List.tryPick (fun (_, call) ->
        match call with Pass -> None | Double -> Some 2 | Redouble -> Some 4 | Bid _ -> Some 1)
    |> Option.defaultValue 1
let legal history seat call =
    match call, lastBid history with
    | Pass, _ -> true
    | Bid (l, s), previous ->
        l >= 1 && l <= 7 && s >= 0 && s <= 4 &&
        (previous |> Option.forall (fun (_, pl, ps) -> (l, s) > (pl, ps)))
    | Double, Some (bidder, _, _) -> side bidder <> side seat && multiplier history = 1
    | Redouble, Some (bidder, _, _) -> side bidder = side seat && multiplier history = 2
    | _ -> false
let auctionOver history =
    let passes = history |> List.takeWhile (fun (_, c) -> c = Pass) |> List.length
    passes >= (if lastBid history = None then 4 else 3)
let opener history =
    history |> List.rev |> List.tryPick (function seat, Bid _ -> Some seat | _ -> None)
let contract history =
    lastBid history |> Option.map (fun (bidder, l, s) ->
        let declarer = history |> List.rev |> List.pick (function
            | p, Bid (_, strain) when side p = side bidder && strain = s -> Some p
            | _ -> None)
        { Level = l; Strain = s; Declarer = declarer; Multiplier = multiplier history })

let hcp hand = hand |> List.sumBy (fun c -> max 0 (c.Rank - 10))
let lengths hand = Array.init 4 (fun s -> hand |> List.sumBy (fun c -> if c.Suit = s then 1 else 0))
let balanced (shape: int array) =
    match Array.sort shape with
    | [| 3; 3; 3; 4 |] | [| 2; 3; 4; 4 |] | [| 2; 3; 3; 5 |] -> true
    | _ -> false
let gameLevel = function 4 -> 3 | 2 | 3 -> 4 | _ -> 5

// Natural Acol subset. Decisions use only this hand and the public auction.
let acol history seat hand =
    let points, shape = hcp hand, lengths hand
    let longest = [0..3] |> List.maxBy (fun s -> shape[s], s)
    let ownCalls = history |> List.choose (fun (p, c) -> if p = seat then Some c else None)
    let ownBids = ownCalls |> List.filter (function Bid _ -> true | _ -> false) |> List.length
    let weOpened = opener history = Some seat
    let partnerOpened = opener history = Some (partner seat)
    let partnerBids =
        history
        |> List.choose (function
            | p, Bid (l, s) when p = partner seat -> Some (l, s)
            | _ -> None)
        |> List.rev
    let isLegal = legal history seat
    let cheapest strain limit =
        [1..limit] |> List.map (fun l -> Bid (l, strain)) |> List.tryFind isLegal
    let choice =
        match lastBid history, partnerBids with
        | None, _ ->
            if points >= 23 then Bid (2, 0)
            elif balanced shape && points >= 20 && points <= 22 then Bid (2, 4)
            elif balanced shape && points >= 12 && points <= 14 then Bid (1, 4)
            elif points >= 12 then Bid (1, longest)
            elif points >= 6 && points <= 10 && shape[longest] = 6 && longest > 0 then Bid (2, longest)
            else Pass
        | Some (bidder, _, _), [] when side bidder <> side seat && List.forall ((=) Pass) ownCalls ->
            if points >= 9 && shape[longest] >= 5 then cheapest longest (if points >= 11 then 2 else 1) |> Option.defaultValue Pass
            else Pass
        | Some (bidder, level, strain), (pl, ps) :: _ when List.forall ((=) Pass) ownCalls ->
            let uncontested = side bidder = side seat
            // Only an opening 2♣ is the strong artificial bid; a 2♣ overcall is natural.
            if pl = 2 && ps = 0 && partnerOpened then
                // 2♦ is the negative, so a diamond positive is 3♦.
                if points >= 8 && shape[longest] >= 5 then
                    Bid ((if longest <= 1 then 3 else 2), longest)
                else Bid (2, 1)
            elif ps = 4 then
                if pl = 1 && points >= 12 then Bid (3, 4)
                elif pl = 1 && points >= 11 && uncontested then Bid (2, 4)
                elif pl = 2 && points >= 5 then Bid (3, 4)
                else Pass
            elif pl = 1 && points >= 6 then
                if shape[ps] >= 4 && (ps >= 2 || points < 10) then
                    let raiseTo = if points >= 13 then gameLevel ps elif points >= 10 then 3 else 2
                    Bid (raiseTo, ps)
                else
                    let newSuits = [0..3] |> List.filter (fun s -> s <> ps && shape[s] >= 4)
                    let oneLevel = newSuits |> List.tryFind (fun s -> isLegal (Bid (1, s)))
                    match oneLevel with
                    | Some s -> Bid (1, s)
                    | None when points >= 10 ->
                        newSuits |> List.sortByDescending (fun s -> shape[s], s)
                        |> List.tryPick (fun s -> if isLegal (Bid (2, s)) then Some (Bid (2, s)) else None)
                        |> Option.defaultValue (if level = 1 then Bid (1, 4) else Pass)
                    | None -> Bid (1, 4)
            elif pl = 2 && ps < 4 && points >= 15 && shape[ps] >= 3 then Bid (gameLevel ps, ps)
            else Pass
        | Some (bidder, level, strain), _ when side bidder = side seat ->
            match ownCalls |> List.rev |> List.tryFind ((<>) Pass), partnerBids with
            // Opener's rebids. Each applies once, to the opening bid.
            | Some (Bid (2, 0)), (responseLevel, response) :: _ when weOpened && ownBids = 1 ->
                let positive = (responseLevel, response) <> (2, 1)
                if positive && response >= 2 && response < 4 && shape[response] >= 3 then Bid (4, response)
                elif balanced shape then Bid (3, 4)
                elif positive && response < 2 && shape[response] >= 3 then Bid (5, response)
                else Bid (gameLevel longest, longest)
            | Some (Bid (1, 4)), [(2, 4)] when weOpened && ownBids = 1 && points = 14 -> Bid (3, 4)
            | Some (Bid (1, opened)), [(_, response)] when weOpened && ownBids = 1 && opened < 4 ->
                if response = 4 && balanced shape && points >= 18 then Bid (3, 4)
                elif response < 4 && shape[response] >= 4 && points >= 16 then Bid (gameLevel response, response)
                elif response = opened && level = 2 && points >= 17 then Bid (gameLevel opened, opened)
                // A 3-level raise is a limit raise (10–12): accept with 14 or more.
                elif response = opened && level = 3 && points >= 14 then
                    if opened < 2 then Bid (3, 4) else Bid (4, opened)
                elif response < 4 && response <> opened then
                    // A new suit is forcing for one round, so opener never passes it.
                    let sameOrNextLevel s = if s > response then level else level + 1
                    [ if response >= 2 && shape[response] >= 4 then Bid (level + 1, response)
                      if shape[opened] >= 5 then Bid (sameOrNextLevel opened, opened)
                      if balanced shape && points >= 15 then
                          Bid (min 3 (if points >= 18 then level + 1 else level), 4)
                      for s in 0..3 do
                          if s <> opened && s <> response && shape[s] >= 4 then Bid (sameOrNextLevel s, s)
                      if shape[response] >= 3 then Bid (level + 1, response)
                      Bid (level, 4) ]
                    |> List.tryFind isLegal |> Option.defaultValue Pass
                else Pass
            | _ -> Pass
        | _ -> Pass
    if isLegal choice then choice else Pass

let legalCards trick hand =
    match trick with
    | [] -> hand
    | (_, lead) :: _ ->
        let followers = hand |> List.filter (fun c -> c.Suit = lead.Suit)
        if List.isEmpty followers then hand else followers
let winner trump (trick: (int * Card) list) =
    let lead = (snd trick.Head).Suit
    trick |> List.maxBy (fun (_, c) ->
        (if c.Suit = trump then 2 elif c.Suit = lead then 1 else 0), c.Rank) |> fst
let botCard trump seat trick hand =
    // Side suits before trumps, then low to high: a void hand discards rather than
    // wasting a trump, and still finds the cheapest winning ruff.
    let choices = legalCards trick hand |> List.sortBy (fun c -> c.Suit = trump, c.Rank)
    match trick with
    | [] ->
        let shape = lengths hand
        choices |> List.maxBy (fun c -> c.Suit <> trump, shape[c.Suit], c.Rank)
    | _ ->
        if winner trump trick = partner seat then choices.Head
        else choices |> List.tryFind (fun c -> winner trump (trick @ [seat, c]) = seat) |> Option.defaultValue choices.Head

let vulnerable board seat =
    let cycle = [| -1; 0; 1; 2; 0; 1; 2; -1; 1; 2; -1; 0; 2; -1; 0; 1 |]
    let v = cycle[(board - 1) % 16]
    v = 2 || v = side seat
let score board contract tricks =
    let extra = tricks - 6 - contract.Level
    let vul = vulnerable board contract.Declarer
    let m = contract.Multiplier
    if extra < 0 then
        let down = -extra
        if m = 1 then -down * (if vul then 100 else 50)
        else
            let doubled =
                if vul then 200 + (down - 1) * 300
                else 100 + min 2 (down - 1) * 200 + max 0 (down - 3) * 300
            -doubled * (m / 2)
    else
        let per = if contract.Strain < 2 then 20 else 30
        let contractPoints = m * (per * contract.Level + if contract.Strain = 4 then 10 else 0)
        let bonus = if contractPoints < 100 then 50 elif vul then 500 else 300
        let slam = if contract.Level = 7 then (if vul then 1500 else 1000)
                   elif contract.Level = 6 then (if vul then 750 else 500) else 0
        let over = if m = 1 then per else (if vul then 200 else 100) * (m / 2)
        contractPoints + bonus + slam + extra * over + (if m = 1 then 0 else 25 * m)
