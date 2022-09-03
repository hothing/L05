module X0Allocation
    open ALGraph
    open GraphColoring
    open X0Lang

    let x0varsAllocate prg =
        match prg with
        | X0Program (_) -> prg

