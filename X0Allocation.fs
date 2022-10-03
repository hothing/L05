module X0Allocation
    open X0BuildInterferences
    open GraphColoring
    open X0Lang

    let x0varsAllocate registers aGraph prg =
        //let aGraph = buildInterferences prg
        let cg = makeColoredGraph aGraph
        // - try color the interference graph with defined amount of reisters
        // - if it is possible then use it
        // - else use allocation on stack, amount of 'stack' vars should be calculated  
        match prg with
        | X0ProgramAbs (_) 
        | X0ProgramImp (_) -> prg

