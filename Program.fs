module Program

open System
open System.Drawing
open System.Windows.Forms
open Bridge

type Phase = Auction | Play | Finished
/// One line of the auction grid: a call (or blank) per seat, North to West.
type AuctionRow =
    { Cells: string array; Spoken: string }
    override row.ToString() = row.Spoken

[<STAThread; EntryPoint>]
let main _ =
    // Render sharply at the system DPI instead of letting Windows stretch a 96-DPI bitmap.
    Application.SetHighDpiMode(HighDpiMode.SystemAware) |> ignore
    Application.EnableVisualStyles()
    Application.SetCompatibleTextRenderingDefault(false)
    // The layout is designed in 96-DPI pixels for a 2000 × 1120 client area (13 cards
    // across, all rows visible). Scale it to the system DPI, then shrink it uniformly
    // if the screen cannot hold it. Font sizes are in points, which already follow the DPI.
    let dpi =
        use g = Graphics.FromHwnd IntPtr.Zero
        g.DpiX / 96.0f
    let area = Screen.FromPoint(Control.MousePosition).WorkingArea
    let fit =
        min 1.0f (min (float32 area.Width / (2000.0f * dpi))
                      (float32 (area.Height - SystemInformation.CaptionHeight) / (1120.0f * dpi)))
    let px (n: int) = int (MathF.Round(float32 n * dpi * fit))
    let pt (size: float32) = size * fit
    let felt = Color.FromArgb(24, 79, 66)
    let dark = Color.FromArgb(17, 57, 49)
    let white = Color.FromArgb(255, 251, 235)
    let form = new Form(Text = "Bridge Table  |  Acol", ClientSize = Size(px 1500, px 960),
                        MinimumSize = Size(px 1100, px 700), StartPosition = FormStartPosition.CenterScreen,
                        WindowState = FormWindowState.Maximized, AutoScaleMode = AutoScaleMode.None,
                        BackColor = felt, ForeColor = white, Font = new Font("Segoe UI", pt 14.0f))
    // Scrolls instead of clipping the lower rows when the window is made small.
    let root = new TableLayoutPanel(Dock = DockStyle.Fill, ColumnCount = 1, RowCount = 7, Padding = Padding(px 16),
                                    AutoScroll = true)
    // The bidding row holds three rows of 82-pixel buttons.
    for h in [48; 200; 300; 44; 250; 200] do root.RowStyles.Add(RowStyle(SizeType.Absolute, float32 (px h))) |> ignore
    root.RowStyles.Add(RowStyle(SizeType.Percent, 100.0f)) |> ignore
    form.Controls.Add root
    let label text =
        new Label(Text = text, Dock = DockStyle.Fill, ForeColor = white, TextAlign = ContentAlignment.MiddleCenter)
    let flow () =
        new FlowLayoutPanel(Dock = DockStyle.Fill, WrapContents = false, AutoScroll = true, Margin = Padding(0))
    let button text width height =
        new Button(Text = text, Width = px width, Height = px height, Margin = Padding(px 2), UseVisualStyleBackColor = false,
                   BackColor = white, ForeColor = dark, FlatStyle = FlatStyle.Flat)
    use cardFont = new Font("Segoe UI", pt 36.0f, FontStyle.Bold)
    use suitFont = new Font("Segoe UI Symbol", pt 56.0f)
    use strainNameFont = new Font("Segoe UI", pt 18.0f, FontStyle.Bold)
    // East and West use small two-line cards: rank above suit.
    use sideRankFont = new Font("Segoe UI", pt 18.0f, FontStyle.Bold)
    use sideSuitFont = new Font("Segoe UI Symbol", pt 18.0f)
    use bidFont = new Font("Segoe UI", pt 24.0f, FontStyle.Bold)
    use statusFont = new Font("Segoe UI", pt 16.0f, FontStyle.Bold)
    use auctionFont = new Font("Segoe UI Semibold", pt 22.0f)
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
    info.Size <- Size(px 850, px 40)
    info.Dock <- DockStyle.None
    toolbar.Controls.AddRange [| dealButton :> Control; hintButton; rulesButton; info |]
    root.Controls.Add(toolbar, 0, 0)

    let makeHand row title =
        let panel = new TableLayoutPanel(Dock = DockStyle.Fill, ColumnCount = 1, RowCount = 2, Margin = Padding(0))
        panel.RowStyles.Add(RowStyle(SizeType.Absolute, float32 (px 28))) |> ignore
        panel.RowStyles.Add(RowStyle(SizeType.Percent, 100.0f)) |> ignore
        let caption = label title
        let cards = flow ()
        panel.Controls.Add(caption, 0, 0)
        panel.Controls.Add(cards, 0, 1)
        root.Controls.Add(panel, 0, row)
        caption, (cards :> Control)
    let northName, northHand = makeHand 1 "North"
    let southName, southHand = makeHand 5 "South"

    let table = new TableLayoutPanel(Dock = DockStyle.Fill, ColumnCount = 3, RowCount = 1, Margin = Padding(0))
    table.ColumnStyles.Add(ColumnStyle(SizeType.Percent, 25.0f)) |> ignore
    table.ColumnStyles.Add(ColumnStyle(SizeType.Percent, 50.0f)) |> ignore
    table.ColumnStyles.Add(ColumnStyle(SizeType.Percent, 25.0f)) |> ignore
    root.Controls.Add(table, 0, 2)
    let flank column title =
        let pane = new TableLayoutPanel(Dock = DockStyle.Fill, ColumnCount = 1, RowCount = 2, Margin = Padding(0))
        pane.RowStyles.Add(RowStyle(SizeType.Absolute, float32 (px 28))) |> ignore
        pane.RowStyles.Add(RowStyle(SizeType.Percent, 100.0f)) |> ignore
        let name = label title
        // Side hands are placed by hand (layoutSide) so all 13 cards fit without scrolling.
        let cards = new Panel(Dock = DockStyle.Fill)
        pane.Controls.Add(name, 0, 0)
        pane.Controls.Add(cards, 0, 1)
        table.Controls.Add(pane, column, 0)
        name, (cards :> Control)
    let westName, westHand = flank 0 "West"
    let eastName, eastHand = flank 2 "East"
    let center = new TableLayoutPanel(Dock = DockStyle.Fill, ColumnCount = 1, RowCount = 2, BackColor = dark,
                                      Margin = Padding(px 8, 0, px 8, 0))
    center.RowStyles.Add(RowStyle(SizeType.Absolute, float32 (px 95))) |> ignore
    center.RowStyles.Add(RowStyle(SizeType.Percent, 100.0f)) |> ignore
    let trickView = label ""
    trickView.Font <- new Font("Segoe UI Semibold", pt 18.0f)
    // The auction is drawn as a grid, one column per seat, as bridge auctions are written.
    let auctionLog = new ListBox(Dock = DockStyle.Fill, BackColor = dark, ForeColor = white, Font = auctionFont,
                                 BorderStyle = BorderStyle.None, IntegralHeight = false, SelectionMode = SelectionMode.None)
    auctionLog.DrawMode <- DrawMode.OwnerDrawFixed
    auctionLog.ItemHeight <- px 40
    let headerColor = Color.FromArgb(160, 200, 185)
    auctionLog.DrawItem.Add(fun e ->
        use background = new SolidBrush(dark)
        e.Graphics.FillRectangle(background, e.Bounds)
        if e.Index >= 0 then
            let row = auctionLog.Items[e.Index] :?> AuctionRow
            let width = e.Bounds.Width / 4
            let color = if e.Index = 0 then headerColor else white
            for i in 0..3 do
                TextRenderer.DrawText(e.Graphics, row.Cells[i], auctionFont,
                                      Rectangle(e.Bounds.X + i * width, e.Bounds.Y, width, e.Bounds.Height), color,
                                      TextFormatFlags.HorizontalCenter ||| TextFormatFlags.VerticalCenter))
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
    let faceDownColor = Color.FromArgb(46, 86, 151)
    let dummySeat () = currentContract |> Option.map (fun c -> partner c.Declarer)
    let isFaceUp seat = seat = 2 || (dummyVisible && dummySeat () = Some seat)
    let canPlayFrom seat = phase = Play && trick.Length < 4 && turn = seat && isHuman seat

    // Card tiles are made once per deal. A redraw removes the tiles of played cards and
    // restyles the others only where their face, colour or playability has changed.
    let shownFaceUp = Array.create 4 false
    let isSide seat = seat = 1 || seat = 3
    let rankText card = (cardName card).Replace(suits[card.Suit], "")
    let makeTile seat card =
        let tile = button "" 144 166
        tile.Tag <- box card
        tile.Paint.Add(fun args ->
            if isFaceUp seat then
                let flags = TextFormatFlags.HorizontalCenter ||| TextFormatFlags.VerticalCenter
                if isSide seat then
                    let half = tile.Height / 2
                    TextRenderer.DrawText(args.Graphics, rankText card, sideRankFont,
                                          Rectangle(0, px 2, tile.Width, half), tile.ForeColor, flags)
                    TextRenderer.DrawText(args.Graphics, suits[card.Suit], sideSuitFont,
                                          Rectangle(0, half - px 2, tile.Width, tile.Height - half), tile.ForeColor, flags)
                else
                    TextRenderer.DrawText(args.Graphics, cardName card, cardFont,
                                          Rectangle(0, px 6, tile.Width, px 60), tile.ForeColor, flags)
                    TextRenderer.DrawText(args.Graphics, suits[card.Suit], suitFont,
                                          Rectangle(0, px 58, tile.Width, px 100), tile.ForeColor, flags))
        tile.Click.Add(fun _ ->
            if not updating && canPlayFrom seat && List.contains card (legalCards trick hands[seat]) then placeCard card)
        tile
    let dealTiles () =
        for seat in 0..3 do
            let _, panel = handViews[seat]
            panel.SuspendLayout()
            clear panel
            for card in List.sortBy displayOrder hands[seat] do panel.Controls.Add(makeTile seat card)
            panel.ResumeLayout()
            // Differs from the real state, so the next redraw styles every new tile.
            shownFaceUp[seat] <- not (isFaceUp seat)
    // East's and West's tiles are sized to their pane so all 13 cards show: face up, in
    // four suit rows (♠ ♥ ♦ ♣) like a hand diagram; face down, as two rows of backs.
    // SetBounds leaves a tile alone when its place is unchanged.
    let layoutSide seat =
        let _, panel = handViews[seat]
        let tiles = panel.Controls |> Seq.cast<Control> |> Seq.toList
        let width, height = panel.ClientSize.Width, panel.ClientSize.Height
        let gap = px 4
        if width > 0 && height > 0 && not tiles.IsEmpty then
            let cardOf (tile: Control) = tile.Tag :?> Card
            if isFaceUp seat then
                let rows =
                    [3; 2; 1; 0] |> List.map (fun s ->
                        tiles |> List.filter (fun t -> (cardOf t).Suit = s) |> List.sortByDescending (fun t -> (cardOf t).Rank))
                let longest = rows |> List.map List.length |> List.max
                let rowHeight = height / 4
                let h = rowHeight - gap
                let w = max (px 30) (min h ((width - gap) / longest - gap))
                let x0 = max 0 ((width - longest * (w + gap) + gap) / 2)
                rows |> List.iteri (fun r row ->
                    row |> List.iteri (fun i t -> t.SetBounds(x0 + i * (w + gap), r * rowHeight + gap / 2, w, h)))
            else
                let columns = 7
                let w = (width - gap) / columns - gap
                let h = min (w * 7 / 5) ((height - gap) / 2 - gap)
                let used = min tiles.Length columns
                let rowCount = (tiles.Length + columns - 1) / columns
                let x0 = max 0 ((width - used * (w + gap) + gap) / 2)
                let y0 = max 0 ((height - rowCount * (h + gap) + gap) / 2)
                tiles |> List.iteri (fun i t -> t.SetBounds(x0 + (i % columns) * (w + gap), y0 + (i / columns) * (h + gap), w, h))
    for seat in [1; 3] do (snd handViews[seat]).Resize.Add(fun _ -> layoutSide seat)
    let updateTiles seat =
        let _, panel = handViews[seat]
        let faceUp = isFaceUp seat
        let playing = canPlayFrom seat
        let legalNow = if playing then legalCards trick hands[seat] else []
        for tile in panel.Controls |> Seq.cast<Control> |> Seq.toList do
            let card = tile.Tag :?> Card
            if not (List.contains card hands[seat]) then tile.Dispose()
            else
                let playable = List.contains card legalNow
                let back = if not faceUp then faceDownColor elif playing && not playable then Color.Silver else white
                let fore = if faceUp && (card.Suit = 1 || card.Suit = 2) then Color.Firebrick else dark
                if tile.BackColor <> back then tile.BackColor <- back
                if tile.ForeColor <> fore then tile.ForeColor <- fore
                tile.TabStop <- playable
                if faceUp <> shownFaceUp[seat] then
                    tile.AccessibleName <- if faceUp then fullCardName card else "Face-down card"
                    tile.Invalidate()
        shownFaceUp[seat] <- faceUp
        if isSide seat then layoutSide seat

    // Bid buttons are made once; a redraw only enables the legal calls.
    let bidButtons =
        [ for call in [ yield Pass; yield Double; yield Redouble
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
                                          Rectangle(0, 0, b.Width, px 43), color, flags)
                    TextRenderer.DrawText(args.Graphics, suitNames[strain], strainNameFont,
                                          Rectangle(0, px 43, b.Width, px 33), color, flags))
            | _ -> ()
            b.Click.Add(fun _ -> if not updating && phase = Auction && isHuman turn then placeCall call)
            bidding.Controls.Add b
            yield call, b ]

    // The auction grid is rebuilt only when a call has been made.
    let mutable shownHistory = [ -1, Pass ]
    let updateAuction () =
        if history <> shownHistory then
            shownHistory <- history
            let calls = List.rev history
            auctionLog.BeginUpdate()
            auctionLog.Items.Clear()
            auctionLog.Items.Add(box { Cells = Array.copy seats; Spoken = "Auction" }) |> ignore
            if not calls.IsEmpty then
                // Blank cells before the dealer put each call under its seat.
                let cells = List.replicate dealer "" @ (calls |> List.map (snd >> callName))
                for chunk in List.chunkBySize 4 cells do
                    let row = Array.ofList (chunk @ List.replicate (4 - chunk.Length) "")
                    let spoken = [ for i in 0..3 do if row[i] <> "" then seats[i] + " " + row[i] ] |> String.concat ", "
                    auctionLog.Items.Add(box { Cells = row; Spoken = spoken }) |> ignore
            auctionLog.TopIndex <- auctionLog.Items.Count - 1
            auctionLog.EndUpdate()

    redraw <- fun () ->
        updating <- true
        form.SuspendLayout()
        try
            let vul = [0; 1] |> List.filter (vulnerable board) |> List.map (function 0 -> "N–S" | _ -> "E–W")
            let vulText = if List.isEmpty vul then "None" else String.concat " / " vul
            info.Text <- sprintf "Board %d   |   Dealer %s   |   Vulnerable %s   |   N–S total %+d" board seats[dealer] vulText total
            hintButton.Enabled <- phase = Auction && isHuman turn
            dealButton.Enabled <- (phase = Finished)
            for seat in 0..3 do
                let caption, _ = handViews[seat]
                caption.Text <- seats[seat] + (if seat = 2 then " · You" else "")
                                + (if dummyVisible && dummySeat () = Some seat then " · Dummy" else "")
                                + (if phase <> Finished && turn = seat then "  ◀" else "")
                updateTiles seat
            updateAuction ()
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
            let showBids = phase = Auction && isHuman turn
            if showBids then
                for call, b in bidButtons do
                    let enabled = legal history turn call
                    if b.Enabled <> enabled then b.Enabled <- enabled
            bidding.Visible <- showBids
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
        dealTiles ()
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
