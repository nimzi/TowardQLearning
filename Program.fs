#nowarn "3391"

open Raylib_cs
open System.IO

// Constants
let screenWidth = 800
let screenHeight = 800
let gridSize = 16
let cellSize = screenWidth / gridSize

// Create 2D array to store cell states (false = white, true = black)
let mutable grid = Array2D.create gridSize gridSize 1


[<Literal>]
let StartState = 2

[<Literal>]
let FinalState = 3


let gridHasStartState() =
    let x = Seq.cast<int> grid
    
    Seq.tryFind (fun i ->  i = StartState) x
    
let asBool (b:CBool) = b <> CBool(false)

let fileName = "grid.pgm"


let saveAsPgm  (path: string) (grid: int[,]) =
    let width = Array2D.length2 grid
    let height = Array2D.length1 grid

    use writer = new StreamWriter(path)

    // Write PGM header
    writer.WriteLine("P2")
    writer.WriteLine($"# Created by F# saveAsPgm")
    writer.WriteLine($"{width} {height}")
    writer.WriteLine("3") // Max grayscale value

    // Write pixel values
    for row = 0 to height - 1 do
        for col = 0 to width - 1 do
            writer.Write($"{grid.[row, col]} ")
        writer.WriteLine()

    writer.Flush()


open System.IO

let loadPgm (path: string) : int[,] =
    let lines =
        File.ReadAllLines(path)
        |> Array.filter (fun line -> not (line.StartsWith("#")) && line.Trim() <> "")

    if lines.[0] <> "P2" then
        failwith "Only ASCII PGM (P2) format is supported"

    let sizeLine = lines.[1].Split([|' '; '\t'|], System.StringSplitOptions.RemoveEmptyEntries)
    let width = int sizeLine.[0]
    let height = int sizeLine.[1]

    // Skip: [0] "P2", [1] dimensions, [2] max grayscale (usually 255)
    let pixelValues =
        lines
        |> Array.skip 3
        |> Array.collect (fun line -> line.Split([|' '; '\t'|], System.StringSplitOptions.RemoveEmptyEntries))
        |> Array.map int

    if pixelValues.Length <> width * height then
        failwith "Unexpected number of pixel values"

    // Fill into 2D array
    let grid = Array2D.zeroCreate height width
    for row in 0 .. height - 1 do
        for col in 0 .. width - 1 do
            grid.[row, col] <- pixelValues.[row * width + col]

    grid




// Try loading a file

try 
    grid <- loadPgm fileName
with 
    | e -> printfn "Unable to load %A" fileName


// Initialize Raylib window
Raylib.InitWindow(screenWidth, screenHeight, "F# Grid Editor")
Raylib.SetTargetFPS(60)

while not (Raylib.WindowShouldClose()) do
    // Input handling
    if Raylib.IsMouseButtonPressed(MouseButton.Left) |> asBool then 
        let mouseX = Raylib.GetMouseX()
        let mouseY = Raylib.GetMouseY()
        let col = mouseX / cellSize
        let row = mouseY / cellSize
        if row < gridSize && col < gridSize then
            grid.[row, col] <- if grid.[row, col] = 0 then 1 else 0 // Toggle cell

    if Raylib.IsKeyPressed(KeyboardKey.S) |> asBool then
        saveAsPgm fileName grid

    if Raylib.IsMouseButtonPressed(MouseButton.Right) |> asBool then 
        let mouseX = Raylib.GetMouseX()
        let mouseY = Raylib.GetMouseY()
        let col = mouseX / cellSize
        let row = mouseY / cellSize
        if row < gridSize && col < gridSize then
            let g = grid[row,col]

            if gridHasStartState().IsSome then
                grid[row, col] <- if g < StartState then FinalState else 0 // Toggle cell
            else 
                grid[row, col] <- if g < StartState then StartState else 0 // Toggle

    // Drawing
    Raylib.BeginDrawing()
    Raylib.ClearBackground(Color.White)

    let color = function
    | 0-> Color.Black
    | 1-> Color.White
    | StartState -> Color.Red
    | FinalState -> Color.Green
    | _ -> Color.Orange

    for row = 0 to gridSize - 1 do
        for col = 0 to gridSize - 1 do
            let x = col * cellSize
            let y = row * cellSize
            let color = color grid.[row, col] 
            Raylib.DrawRectangle(x, y, cellSize, cellSize, color)
            Raylib.DrawRectangleLines(x, y, cellSize, cellSize, Color.LightGray) // Grid lines

    Raylib.EndDrawing()

Raylib.CloseWindow()
