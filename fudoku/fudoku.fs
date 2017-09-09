module fudoku

type SudokuValue =
    | Empty
    | Guess of int
    | Value of int

type List<'T> with
    static member replaceAt idx v l =
        let (l1,l2) = List.splitAt idx l 
        match l2 with
        | [] -> l1
        | h::tail ->
            let r2 = v::tail
            List.concat [l1; r2]



type SudokuValue with
     member s.String =
        match s with
        | Empty -> " * "
        | Value i -> sprintf " %d " i
        | Guess _ -> sprintf "***"

type Sudoku = SudokuValue list



//genSudoku maps 0 to Empty, and everything else is mapped as Value
let genSudoku values = 
    let convertValue i =
        match i with
        | 0 -> Empty
        | i -> Value i
    values
    |> List.map convertValue
    

let printRow row =
    printfn "" |> ignore
    row 
    |> List.map<SudokuValue,unit> (fun v ->
        let v = v.String
        printf " %A " v)
    |> ignore
    printfn "" |> ignore
    
let rec printSudoku s =
    match s with
    | [] -> ()
    | _ ->
        if s.Length = 9 then 
            s |> printRow |> ignore
            String.replicate 62 "_" |> printfn "%s" |> ignore
        else        
            let (current,remainder) = List.splitAt 9 s
            printRow current
            if remainder.Length % 27 = 0 then
                String.replicate 62 "_" |> printfn "%s" |> ignore
            printSudoku remainder

let getRow (s:Sudoku, r:int) = 
    let skipCount = r * 9
    let skipped = List.skip skipCount s
    List.take 9 skipped
    

let getCol (s:Sudoku, c:int) = 
    let rows = [0..8] |> List.map (fun ri -> getRow (s,ri) )
    rows 
    |> List.map (fun r -> 
        let skipRow = List.skip c r        
        let result =  List.head  skipRow
        result
    )


let simpleReducer(r:SudokuValue list) = 
        r

let transpose s = 
    let transposed = 
        [0..8] 
        |> List.map (fun i -> getCol(s,i))
    List.collect simpleReducer transposed
        


let getSquare (s:Sudoku, c:int) =
    let (row,col) = c/3, c%3
    let rowsIdx = [0..2] |> List.map (fun (v) -> row*3 + v)
    let rows = rowsIdx |> List.map ( fun (r) -> getRow(s,r)) 
    let colStart = col*3
    rows 
    |> List.collect (fun r ->
        let skipList = List.skip colStart r
        List.take 3 skipList
    )

type SudokuSolver = {s: Sudoku; transposed: bool}


//Returns rows at s[idx*3 +0, idx*3 +1, idx*3 +2]
//in a flatten list
let scanRowSet s idx =
    let initialRow = idx * 3 
    let rowIdxs = [0..2] |> List.map (fun v -> initialRow + v)
    let rows = rowIdxs |> List.map (fun ri -> getRow(s,ri) ) 
    
    List.collect simpleReducer rows

//getRepeatedInRow returns the numbers that are
//repeated into the rows s[idx*3 : idx*3 +2] exactly 2 times
let repeatedTwoTimesInRow s idx = 
    let rows = scanRowSet s idx
    let counts = List.countBy id rows 
    let doubledVals = counts |> List.filter (fun (k,v) -> v = 2 )
    List.map ( fun (k,v) -> k) doubledVals

//isInRow returns true if the element v is in s[row]
let isInRow v row = 
    let find = List.tryFind (fun c -> c = v ) row
    match find with
    | Some _ -> true
    | None -> false

let getSquareMissingValueIdx s idx v = 
    let sqInitial = idx *3 
    let squares = [0..2] |> List.map (fun v -> sqInitial + v)     
    squares 
    |> List.map ((fun sq -> getSquare(s,sq)) >> (fun r -> isInRow v r))
    |> List.findIndex (fun v -> v = false)

let getRowMissingValueIdx s idx v = 
    let rowInitial = idx *3 
    let rows = [0..2] |> List.map (fun v -> rowInitial + v)     
    rows 
    |> List.map ((fun sq -> getRow(s,sq)) >> (fun r -> isInRow v r))
    |> List.findIndex (fun v -> v = false)


let filterIsEmpty (idx,elem) =
    match elem with
    | Value v -> false
    | _ -> true

//it checks for the empty values in the square sq 
//in the row between [0..2]
let checkEmptyValues row sq s=
    let skipValues = row*3
    getSquare(s,sq)
    |> List.skip skipValues 
    |> List.take 3
    |> List.indexed
    |> List.filter filterIsEmpty 

let mutate position value s = 
    List.replaceAt position value s

let getRealRow sq relativeRow = 
    let absRow = (sq/3)
    (absRow + relativeRow)

let getRealCol sq relativeCol =
    let absCol = (sq % 3) 
    (3*absCol) + relativeCol

let getPosition sq relativeRow relativeCol = 
    let (r,c) = (getRealRow sq relativeRow, getRealCol sq relativeCol)
    r*9 + c

let checkEmpties s sq v row empties = 
    match empties with
        | [(col,_)] -> 
            //one perfect match means the value goes here
            let position = getPosition sq row col
            let newS = List.replaceAt position v s
            printfn "New sudoku"
            newS |> printSudoku
            (newS,true)
        | _ ->
            //checks per each available column, the value is only required
            //in one of them
            let checkByCol = (fun (colIdx,_) -> 
                let realCol = getRealCol sq colIdx
                let col = getCol(s,realCol)
                (isInRow v col,colIdx)
            )
            let isInCols = List.map checkByCol empties 
            let missingValues = isInCols |> List.filter (fun (ok,_) -> ok = false)
            if missingValues.Length = 1 then
                let (_,col) = missingValues.Head 
                let position = getPosition sq row col
                let newS = List.replaceAt position v s
                printfn "New sudoku"
                newS |> printSudoku
                (newS,true)
            else  
                (s,false)   

let rec checkRepeated s repeated idx = 
    match repeated with
    | [] -> (s,false)
    | v::remainder ->
        let availableRow = getRowMissingValueIdx s idx v
        let availableSq = idx*3 + (getSquareMissingValueIdx s idx v)
        printfn "value %A, Row %d Sq %d" v availableRow availableSq
        let empties = checkEmptyValues availableRow availableSq s
        printfn "Empties for this value %A" empties
        let (s, mutated) = checkEmpties s availableSq v availableRow empties
        checkRepeated s remainder idx

let rec checkByRow s idx = 
    let repeated = repeatedTwoTimesInRow s idx
    checkRepeated s repeated idx
    
               


[<EntryPoint>]
let main argv =
    let values = [0;0;7;0;0;5;6;1;0;
                  4;0;0;9;0;1;0;0;0;
                  1;0;0;0;3;6;0;0;2;
                  3;7;2;0;0;0;0;4;0;
                  0;0;9;0;0;0;1;0;0;
                  0;1;0;0;0;0;7;2;9;
                  5;0;0;8;7;0;0;0;4;
                  0;0;0;5;0;4;0;0;7;
                  0;9;4;3;0;0;8;0;0]
    let s = genSudoku values |> transpose
    printSudoku s
    printfn "***"
    // [0..8]
    // |> List.map( fun v -> 
    //     let row = getCol (s,v)
    //     printRow row
    //     ()
    // )
    // |> ignore
    let row = 0 
    let (s,checkAgain) = checkByRow s 0
    match checkAgain with
    | true -> checkByRow s 0 |> ignore
    | false -> ()
    0 // return an integer exit code


