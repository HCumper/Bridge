# Bridge Table

A single-player contract bridge table written in F# and Windows Forms. Requires Windows and the .NET 10 Desktop Runtime. Building from source requires the .NET 10 SDK.

Run `dotnet run --project Bridge.fsproj` from this directory. To build a copy you can launch directly, run `dotnet publish Bridge.fsproj -c Release -o app`, then start `app\Bridge.exe`.

Run `dotnet fsi verify.fsx` to check call and card legality over 500 random deals, the scoring table, and known bidding and card-play cases.

The table scales to the screen's DPI and shrinks to fit screens too small for 13 full-size cards across.

- You sit South and partner North. The three other seats bid and play automatically.
- Choose a call when the auction reaches South. The Acol hint uses your hand and the public auction.
- On your turn, click a card. Cards that cannot follow suit are shaded. If you declare, you also play North's dummy. If North declares, North controls both hands. Dummy becomes visible after the opening lead.
- Play through thirteen tricks, then select **New board**. The program rotates dealer and vulnerability and totals raw duplicate points for North–South.

The computer uses a simplified natural Acol system with four-card majors, a balanced 12–14 1NT, balanced 20–22 2NT, strong 2♣ (2♦ negative), and weak two openings in diamonds, hearts and spades. A new-suit response is forcing, a 3-level raise is a limit raise, and a 2♣ overcall is natural. Its responses, rebids, overcalls and card play use basic heuristics. The computer does not use Stayman, transfers, slam conventions, or competitive doubles. Human players can make any legal call, including doubles and redoubles.

- [English Bridge Union introduction to Acol](https://www.ebu.co.uk/education/learn-play-bridge-acol-edition)
- [Laws of Duplicate Bridge and scoring table](https://cdn.acbl.org/documentLibrary/play/Laws-of-Duplicate-Bridge.pdf)
