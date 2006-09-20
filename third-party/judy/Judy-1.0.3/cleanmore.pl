my @leftover = qw(
    Judy1\Judy1Tables.c
    JudyL\JudyLTables.c
    Judy1\Judy1ByCount.c
    Judy1\Judy1Cascade.c
    Judy1\Judy1Count.c
    Judy1\Judy1CreateBranch.c
    Judy1\Judy1Decascade.c
    Judy1\Judy1Unset.c
    Judy1\Judy1First.c
    Judy1\Judy1FreeArray.c
    Judy1\Judy1Test.c
    Judy1\j__udy1Test.c
    Judy1\Judy1SetArray.c
    Judy1\Judy1Set.c
    Judy1\Judy1InsertBranch.c
    Judy1\Judy1MallocIF.c
    Judy1\Judy1MemActive.c
    Judy1\Judy1MemUsed.c
    Judy1\Judy1Next.c
    Judy1\Judy1Prev.c
    Judy1\Judy1NextEmpty.c
    Judy1\Judy1PrevEmpty.c
    Judy1\Judy1TablesGen.c
    JudyL\JudyLByCount.c
    JudyL\JudyLCascade.c
    JudyL\JudyLCount.c
    JudyL\JudyLCreateBranch.c
    JudyL\JudyLDecascade.c
    JudyL\JudyLDel.c
    JudyL\JudyLFirst.c
    JudyL\JudyLFreeArray.c
    JudyL\JudyLGet.c
    JudyL\j__udyLGet.c
    JudyL\JudyLInsArray.c
    JudyL\JudyLIns.c
    JudyL\JudyLInsertBranch.c
    JudyL\JudyLMallocIF.c
    JudyL\JudyLMemActive.c
    JudyL\JudyLMemUsed.c
    JudyL\JudyLNext.c
    JudyL\JudyLPrev.c
    JudyL\JudyLNextEmpty.c
    JudyL\JudyLPrevEmpty.c
    JudyL\JudyLTablesGen.c
    Judy1\Judy1TablesGen
    JudyL\JudyLTablesGen
    Makefile
);

for (@leftover) {
    unlink "src/$_";
}
unlink "Makefile";
unlink "config.status";
warn "info: Leftover files removed.\n";
