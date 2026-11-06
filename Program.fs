module Program

open System
open System.Drawing
open System.Windows.Forms
open Bridge

type Phase = Auction | Play | Finished

[<STAThread; EntryPoint>]
let main _ =
    Application.EnableVisualStyles()
    Application.SetCompatibleTextRenderingDefault(false)
    let felt = Color.FromArgb(24, 79, 66)
    let dark = Color.FromArgb(17, 57, 49)
    let white = Color.FromArgb(255, 251, 235)
    let form = new Form(Text = "Bridge Table  |  Acol", ClientSize = Size(1500, 1040),
                        MinimumSize = Size(1100, 900), StartPosition = FormStartPosition.CenterScreen,
                        WindowState = FormWindowState.Maximized,
                        BackColor = felt, ForeColor = white, Font = new Font("Segoe UI", 14.0f))
    let root = new TableLayoutPanel(Dock = DockStyle.Fill, ColumnCount = 1, RowCount = 7, Padding = Padding(16))
    for h in [48; 200; 220; 44; 210; 200] do root.RowStyles.Add(RowStyle(SizeType.Absolute, float32 h)) |> ignore
    root.RowStyles.Add(RowStyle(SizeType.Percent, 100.0f)) |> ignore
    form.Controls.Add root
    let label text =
        new Label(Text = text, Dock = DockStyle.Fill, ForeColor = white, TextAlign = ContentAlignment.MiddleCenter)
    let flow () =
        new FlowLayoutPanel(Dock = DockStyle.Fill, WrapContents = false, AutoScroll = true, Margin = Padding(0))
    let button text width height =
        new Button(Text = text, Width = width, Height = height, Margin = Padding(2), UseVisualStyleBackColor = false,
                   BackColor = white, ForeColor = dark, FlatStyle = FlatStyle.Flat)
    use cardFont = new Font("Segoe UI", 36.0f, FontStyle.Bold)
    use suitFont = new Font("Segoe UI Symbol", 46.0f)
    use cardNameFont = new Font("Segoe UI", 18.0f, FontStyle.Bold)
    use bidFont = new Font("Segoe UI", 24.0f, FontStyle.Bold)
    use statusFont = new Font("Segoe UI", 16.0f, FontStyle.Bold)
    let displaySuitOrder = [| 0; 1; 3; 2 |]
    let displayOrder card = Array.findIndex ((=) card.Suit) displaySuitOrder, -card.Rank
    let suitNames = [| "Clubs"; "Diamonds"; "Hearts"; "Spades"; "No trump" |]
    let rankName rank =
        [| "Two"; "Three"; "Four"; "Five"; "Six"; "Seven"; "Eight"; "Nine";
           "Ten"; "Jack"; "Queen"; "King"; "Ace" |][rank - 2]
    let fullCardName card = rankName card.Rank + " of " + suitNames[card.Suit].ToLowerInvariant()
    let toolbar = flow ()
    let dealButton = button "New board" 145 40
    let hintButton = button "Acol hint" 135 40
    let rulesButton = button "System & rules" 185 40
    let info = label ""
    info.AutoSize <- false
    info.Size <- Size(850, 40)
    info.Dock <- DockStyle.None
    toolbar.Controls.AddRange [| dealButton :> Control; hintButton; rulesButton; info |]
    root.Controls.Add(toolbar, 0, 0)

    let makeHand row title =
        let panel = new TableLayoutPanel(Dock = DockStyle.Fill, ColumnCount = 1, RowCount = 2, Margin = Padding(0))
        panel.RowStyles.Add(RowStyle(SizeType.Absolute, 28.0f)) |> ignore
        panel.RowStyles.Add(RowStyle(SizeType.Percent, 100.0f)) |> ignore
        let caption = label title
        let cards = flow ()
        panel.Controls.Add(caption, 0, 0)
        panel.Controls.Add(cards, 0, 1)
        root.Controls.Add(panel, 0, row)
        caption, cards
    let northName, northHand = makeHand 1 "North"
    let southName, southHand = makeHand 5 "South"

    let table = new TableLayoutPanel(Dock = DockStyle.Fill, ColumnCount = 3, RowCount = 1, Margin = Padding(0))
    table.ColumnStyles.Add(ColumnStyle(SizeType.Percent, 25.0f)) |> ignore
    table.ColumnStyles.Add(ColumnStyle(SizeType.Percent, 50.0f)) |> ignore
    table.ColumnStyles.Add(ColumnStyle(SizeType.Percent, 25.0f)) |> ignore
    root.Controls.Add(table, 0, 2)
    let flank column title =
        let pane = new TableLayoutPanel(Dock = DockStyle.Fill, ColumnCount = 1, RowCount = 2, Margin = Padding(0))
        pane.RowStyles.Add(RowStyle(SizeType.Absolute, 28.0f)) |> ignore
        pane.RowStyles.Add(RowStyle(SizeType.Percent, 100.0f)) |> ignore
        let name = label title
        let cards = new FlowLayoutPanel(Dock = DockStyle.Fill, AutoScroll = true)
        pane.Controls.Add(name, 0, 0)
        pane.Controls.Add(cards, 0, 1)
        table.Controls.Add(pane, column, 0)
        name, cards
    let westName, westHand = flank 0 "West"
    let eastName, eastHand = flank 2 "East"
    let center = new TableLayoutPanel(Dock = DockStyle.Fill, ColumnCount = 1, RowCount = 2, BackColor = dark, Margin = Padding(8, 0, 8, 0))
    center.RowStyles.Add(RowStyle(SizeType.Absolute, 95.0f)) |> ignore
    center.RowStyles.Add(RowStyle(SizeType.Percent, 100.0f)) |> ignore
    let trickView = label ""
    trickView.Font <- new Font("Segoe UI Semibold", 18.0f)
    let auctionLog = new ListBox(Dock = DockStyle.Fill, BackColor = dark, ForeColor = white,
                                 BorderStyle = BorderStyle.None, IntegralHeight = false)
    center.Controls.Add(trickView, 0, 0)
    center.Controls.Add(auctionLog, 0, 1)
    table.Controls.Add(center, 1, 0)
    let status = label ""
    status.Font <- statusFont
    root.Controls.Add(status, 0, 3)
    let bidding = new FlowLayoutPanel(Dock = DockStyle.Fill, WrapContents = true, Margin = Padding(0), AutoScroll = true)
    root.Controls.Add(bidding, 0, 4)
    let summary = label ""
    root.Controls.Add(summary, 0, 6)

    let timer = new Timer()
    let mutable board = 0
    let mutable dealer = 0
    let mutable turn = 0
    let mutable phase = Finished
    let mutable hands: Card list array = Array.create 4 []
    let mutable history: (int * Call) list = []
    let mutable currentContract: Contract option = None
    let mutable trick: (int * Card) list = []
    let mutable lastTrick: (int * Card) list = []
    let mutable tricks = [| 0; 0 |]
    let mutable dummyVisible = false
    let mutable total = 0
    let mutable outcome = ""
    let mutable hint = ""
    let mutable played = 0
    let mutable updating = false
    let handViews = [| northName, northHand; eastName, eastHand; southName, southHand; westName, westHand |]
    let isHuman seat =
        match phase, currentContract with
        | Auction, _ -> seat = 2
        | Play, Some c ->
            if c.Declarer = 2 then seat = 2 || seat = 0
            else seat = 2 && partner c.Declarer <> 2
        | _ -> false
    let describe c =
        callName (Bid (c.Level, c.Strain)) + (if c.Multiplier = 2 then "X" elif c.Multiplier = 4 then "XX" else "")
        + " by " + seats[c.Declarer]
    let announce () =
        MessageBox.Show(form, outcome + "\n\nNorth–South total: " + string total
                        + "\n\nChoose New board to continue.",
                        "Board " + string board + " result",
                        MessageBoxButtons.OK, MessageBoxIcon.Information) |> ignore
    let mutable redraw = ignore
    let mutable schedule = ignore

    let placeCall call =
        if phase = Auction && legal history turn call then
            hint <- ""
            history <- (turn, call) :: history
            if auctionOver history then
                currentContract <- contract history
                match currentContract with
                | None -> phase <- Finished; outcome <- "Passed out. Score 0."
                | Some c -> phase <- Play; turn <- next c.Declarer
            else turn <- next turn
            redraw ()
            schedule ()
            if phase = Finished then announce ()

    let placeCard card =
        if phase = Play && trick.Length < 4 && List.contains card (legalCards trick hands[turn]) then
            hands[turn] <- List.filter ((<>) card) hands[turn]
            trick <- trick @ [turn, card]
            played <- played + 1
            dummyVisible <- true
            if trick.Length < 4 then turn <- next turn
            redraw ()
            schedule ()

    let finishTrick () =
        let c = currentContract.Value
        turn <- winner c.Strain trick
        tricks[side turn] <- tricks[side turn] + 1
        lastTrick <- trick
        trick <- []
        if played = 52 then
            let won = tricks[side c.Declarer]
            let difference = won - c.Level - 6
            let declarerScore = score board c won
            let nsScore = if side c.Declarer = 0 then declarerScore else -declarerScore
            total <- total + nsScore
            let result =
                if difference = 0 then "Contract made exactly."
                elif difference > 0 then sprintf "Contract made with %d overtrick(s)." difference
                else sprintf "Contract defeated by %d trick(s)." (-difference)
            outcome <- sprintf "%s. %s Declarer took %d of 13 tricks. North–South score: %+d."
                        (describe c) result won nsScore
            phase <- Finished
        redraw ()
        schedule ()
        if phase = Finished then announce ()

    schedule <- fun () ->
        timer.Stop()
        if phase <> Finished && (trick.Length = 4 || not (isHuman turn)) then
            timer.Interval <- if trick.Length = 4 then 1050 else 380
            timer.Start()
    timer.Tick.Add(fun _ ->
        timer.Stop()
        match phase with
        | Auction when not (isHuman turn) -> placeCall (acol history turn hands[turn])
        | Play when trick.Length = 4 -> finishTrick ()
        | Play when not (isHuman turn) -> placeCard (botCard currentContract.Value.Strain turn trick hands[turn])
        | _ -> ())
    form.FormClosed.Add(fun _ -> timer.Dispose())

    let clear (panel: Control) =
        while panel.Controls.Count > 0 do panel.Controls[0].Dispose()
    redraw <- fun () ->
        updating <- true
        form.SuspendLayout()
        try
            let vul = [0; 1] |> List.filter (vulnerable board) |> List.map (function 0 -> "N–S" | _ -> "E–W")
            let vulText = if List.isEmpty vul then "None" else String.concat " / " vul
            info.Text <- sprintf "Board %d   |   Dealer %s   |   Vulnerable %s   |   N–S total %+d" board seats[dealer] vulText total
            hintButton.Enabled <- phase = Auction && isHuman turn
            dealButton.Enabled <- (phase = Finished)
            let dummy = currentContract |> Option.map (fun c -> partner c.Declarer)
            for seat in 0..3 do
                let caption, panel = handViews[seat]
                let faceUp = seat = 2 || (dummyVisible && dummy = Some seat)
                let canPlay = phase = Play && trick.Length < 4 && turn = seat && isHuman seat
                caption.Text <- seats[seat] + (if seat = 2 then " · You" else "")
                                + (if dummyVisible && dummy = Some seat then " · Dummy" else "")
                                + (if phase <> Finished && turn = seat then "  ◀" else "")
                clear panel
                let legalNow = if canPlay then legalCards trick hands[seat] else []
                for card in List.sortBy displayOrder hands[seat] do
                    let tile = button "" 144 166
                    tile.Font <- cardFont
                    tile.AccessibleName <- if faceUp then fullCardName card else "Face-down card"
                    if faceUp then
                        tile.Paint.Add(fun args ->
                            let color = tile.ForeColor
                            let flags = TextFormatFlags.HorizontalCenter ||| TextFormatFlags.VerticalCenter
                            TextRenderer.DrawText(args.Graphics, cardName card, cardFont,
                                                  Rectangle(0, 0, tile.Width, 55), color, flags)
                            TextRenderer.DrawText(args.Graphics, suits[card.Suit], suitFont,
                                                  Rectangle(0, 48, tile.Width, 61), color, flags)
                            TextRenderer.DrawText(args.Graphics, rankName card.Rank + "\nof " + suitNames[card.Suit].ToLowerInvariant(),
                                                  cardNameFont, Rectangle(0, 109, tile.Width, 52), color, flags))
                    if not faceUp then tile.BackColor <- Color.FromArgb(46, 86, 151)
                    elif card.Suit = 1 || card.Suit = 2 then tile.ForeColor <- Color.Firebrick
                    if canPlay && not (List.contains card legalNow) then tile.BackColor <- Color.Silver
                    tile.TabStop <- List.contains card legalNow
                    if List.contains card legalNow then
                        tile.Click.Add(fun _ -> if not updating && turn = seat && isHuman seat then placeCard card)
                    panel.Controls.Add tile
            auctionLog.BeginUpdate()
            auctionLog.Items.Clear()
            for seat, call in List.rev history do
                auctionLog.Items.Add(sprintf "%-6s   %s" seats[seat] (callName call)) |> ignore
            if auctionLog.Items.Count > 0 then auctionLog.TopIndex <- auctionLog.Items.Count - 1
            auctionLog.EndUpdate()
            let shown, title = if List.isEmpty trick then lastTrick, "Last trick" else trick, "Current trick"
            trickView.Text <-
                if List.isEmpty shown then currentContract |> Option.map describe |> Option.defaultValue "Auction"
                else title + "\n" + (shown |> List.map (fun (seat, card) -> seats[seat] + " " + cardName card) |> String.concat "   ")
            status.Text <-
                match phase with
                | Auction when isHuman turn -> "Your call · " + string (hcp hands[2]) + " HCP" + hint
                | Auction -> seats[turn] + " is bidding"
                | Play when trick.Length = 4 -> seats[winner currentContract.Value.Strain trick] + " wins the trick"
                | Play when isHuman turn -> "Choose a card from " + seats[turn] + "'s hand"
                | Play -> seats[turn] + " is playing"
                | Finished -> outcome
            summary.Text <-
                match currentContract with
                | Some c -> sprintf "%s   |   Tricks: N–S %d · E–W %d" (describe c) tricks[0] tricks[1]
                | None -> "South partners North. Hearts and spades are majors."
            clear bidding
            if phase = Auction && isHuman turn then
                for call in [ yield Pass; yield Double; yield Redouble
                              for l in 1..7 do for s in [0; 1; 3; 2; 4] -> Bid (l, s) ] do
                    let text =
                        match call with
                        | Bid (level, strain) -> string level + " " + suits[strain] + "\n" + suitNames[strain]
                        | _ -> callName call
                    let b = button (match call with Bid _ -> "" | _ -> text) (match call with Bid _ -> 140 | _ -> 175) 78
                    b.Font <- bidFont
                    b.AccessibleName <- text.Replace("\n", " ")
                    match call with
                    | Bid (_, strain) ->
                        b.Paint.Add(fun args ->
                            let flags = TextFormatFlags.HorizontalCenter ||| TextFormatFlags.VerticalCenter
                            let color = if b.Enabled then dark else SystemColors.GrayText
                            TextRenderer.DrawText(args.Graphics, callName call, bidFont,
                                                  Rectangle(0, 0, b.Width, 43), color, flags)
                            TextRenderer.DrawText(args.Graphics, suitNames[strain], cardNameFont,
                                                  Rectangle(0, 43, b.Width, 33), color, flags))
                    | _ -> ()
                    b.Enabled <- legal history turn call
                    b.Click.Add(fun _ -> if not updating && isHuman turn then placeCall call)
                    bidding.Controls.Add b
        finally
            form.ResumeLayout(true)
            updating <- false

    let newBoard () =
        board <- board + 1
        dealer <- (board - 1) % 4
        turn <- dealer
        hands <- deal ()
        history <- []
        currentContract <- None
        trick <- []
        lastTrick <- []
        tricks <- [| 0; 0 |]
        dummyVisible <- false
        played <- 0
        outcome <- ""
        hint <- ""
        phase <- Auction
        redraw ()
        schedule ()
    dealButton.Click.Add(fun _ -> if phase = Finished then newBoard ())
    hintButton.Click.Add(fun _ ->
        if phase = Auction && isHuman turn then
            hint <- "   |   Acol suggestion: " + callName (acol history turn hands[turn])
            redraw ())
    rulesButton.Click.Add(fun _ ->
        MessageBox.Show(form,
            "You sit South. Click a bid or a legal card when it is your turn.\n\n"
            + "When you declare, you also play North's dummy. When North declares, the computer plays both hands. Dummy appears after the opening lead.\n\n"
            + "The computer uses a simplified natural Acol system: 12–14 balanced 1NT, four-card majors, strong 2♣, balanced 20–22 2NT, and weak twos in diamonds, hearts and spades. Responses, rebids and overcalls are basic.\n\n"
            + "Boards use duplicate scoring. N–S totals are raw points across deals.",
            "System and rules", MessageBoxButtons.OK, MessageBoxIcon.Information) |> ignore)
    form.Shown.Add(fun _ -> newBoard ())
    Application.Run form
    0
