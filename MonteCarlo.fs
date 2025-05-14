module GridworldMC

open System
open System.Collections.Generic

type Action =
    | Up
    | Down
    | Left
    | Right

let private actions = [ Up; Down; Left; Right ]
let private shortName = function
    | Up -> "↑" | Down -> "↓" | Left -> "←" | Right -> "→"

type State = int * int

let private gridSize = 4
let private goal = (3, 3)
let private gamma = 0.9
let private rng = Random()

let blocked : Set<State> = set [ (1,1); (2,2); (3,0) ]  // Example


// let private legalActions ((x, y): State) : Action list =
//     [ if x > 0 then yield Up
//       if x < gridSize - 1 then yield Down
//       if y > 0 then yield Left
//       if y < gridSize - 1 then yield Right ]

let private legalActions ((x, y) : State) : Action list =
    [ if x > 0         && not (blocked.Contains (x - 1, y)) then yield Up
      if x < gridSize - 1 && not (blocked.Contains (x + 1, y)) then yield Down
      if y > 0         && not (blocked.Contains (x, y - 1)) then yield Left
      if y < gridSize - 1 && not (blocked.Contains (x, y + 1)) then yield Right ]

          
type QTable() =
    let q = Dictionary<State, Dictionary<Action, float>>()
    let counts = Dictionary<State, Dictionary<Action, int>>() // visit counts

    member _.Ensure(state: State) =
        if not (q.ContainsKey state) then
            q[state] <- Dictionary(actions |> Seq.map (fun a -> KeyValuePair (a, 0.0)))
            counts[state] <- Dictionary(actions |> Seq.map (fun a -> KeyValuePair(a, 0)))

    member _.Update(state: State, action: Action, G: float) =
        let prevQ = q[state][action]
        let count = counts[state][action] + 1
        counts[state][action] <- count

        // Average of all rewards
        q[state][action] <- prevQ + (G - prevQ) / float count

    member _.Get(state: State, action: Action) =
        if q.ContainsKey state && q[state].ContainsKey action then q[state][action]
        else 0.0

    member _.BestAction(state: State) =
        if q.ContainsKey state then
            q[state] |> Seq.maxBy (fun kv -> kv.Value) |> fun kv -> kv.Key
        else
            let validActions = legalActions state
            validActions[rng.Next validActions.Length]


// Environment and Policy Logic
// let private step ((x, y): State) (action: Action) =
//     let x', y' =
//         match action with
//         | Up -> max 0 (x - 1), y
//         | Down -> min (gridSize - 1) (x + 1), y
//         | Left -> x, max 0 (y - 1)
//         | Right -> x, min (gridSize - 1) (y + 1)
//     let reward = if (x', y') = goal then 10 else -1
//     (x', y'), reward


let private step (state: State) (action: Action) : State * int =
    let tryMove =
        match state, action with
        | (x, y), Up    when x > 0         -> (x - 1, y)
        | (x, y), Down  when x < gridSize - 1 -> (x + 1, y)
        | (x, y), Left  when y > 0         -> (x, y - 1)
        | (x, y), Right when y < gridSize - 1 -> (x, y + 1)
        | _ -> state  // Illegal move: stay put

    let nextState =
        if blocked.Contains tryMove then state else tryMove

    let reward = if nextState = goal then 10 else -1
    nextState, reward



let private policy = Dictionary<State, Action>()
let private qTable = QTable()



let private chooseAction state =
    let validActions = legalActions state
    if rng.NextDouble() < 0.1 then
        validActions[rng.Next validActions.Length]
    else
        if policy.ContainsKey state then policy[state]
        else qTable.BestAction state

// Training Function
let train episodes =
    for _ in 1 .. episodes do
        let mutable state = (0, 0)
        let episode = ResizeArray<State * Action * int>()

        while state <> goal do
            qTable.Ensure state
            let action = chooseAction state
            let nextState, reward = step state action
            episode.Add(state, action, reward)
            state <- nextState

        let mutable G = 0.0
        // every visit Monte Carlo vs first-visit
        // in first-vist we update each Q-table entry only once
        let visited = HashSet<State * Action>()
        for i = episode.Count - 1 downto 0 do
            let s, a, r = episode[i]
            G <- gamma * G + float r
            if visited.Add((s, a)) then
                qTable.Update(s, a, G)
                policy[s] <- qTable.BestAction s

// Run Policy Output
// let runPolicy () =
//     for i in 0 .. gridSize - 1 do
//         for j in 0 .. gridSize - 1 do
//             let s = (i, j)
//             if s = goal then
//                 printf " G  "
//             else
//                 printf " %s  " (
//                     if policy.ContainsKey s then shortName policy[s] else "?"
//                 )
//         printfn ""

let runPolicy () =
    for i in 0 .. gridSize - 1 do
        for j in 0 .. gridSize - 1 do
            let s = (i, j)
            if s = goal then
                printf " G  "
            elif blocked.Contains s then
                printf " ■  "
            else
                printf " %s  " (if policy.ContainsKey s then shortName policy[s] else "?")
        printfn ""
