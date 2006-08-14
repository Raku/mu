@echo off

rem WARMING!!! Don't Edit This File!!! Pugs' Build System Depends On This!

echo  "--- This is a compile kit to suggest how to port to your machine"
echo  "--- This script runs in 7 seconds on a 3.2Ghz Pentium P4C"
echo  "--- Must be in the 'src' directory to execute this script"
echo  "--- I normally run it like: COPT='-O3 -march=i686' sh_build"

echo  "--- Set Compiler"
rem set CC=D:\ghc\ghc-6.4.2\gcc.exe

echo  "--- Set Optimization"

rem COPT='-O'
rem set COPT=-ID:\ghc\ghc-6.4.2\include\mingw -ID:\ghc\ghc-6.4.2\gcc-lib\include -BD:\ghc\ghc-6.4.2\gcc-lib

echo  "--- Set Shared library option"
rem set CPIC=-fPIC
set CPIC=

echo "--- Compile JudyMalloc - common to Judy1 and JudyL"
echo "--- cd JudyCommon"
cd JudyCommon
del *.o
%CC%  %COPT% %CPIC% -I. -I.. -c JudyMalloc.c 
echo "--- cd .."
cd ..

echo "--- Give Judy1 the proper names"
echo "--- cd Judy1"
cd Judy1
del *.o
copy ..\JudyCommon\JudyByCount.c      	Judy1ByCount.c   
copy ..\JudyCommon\JudyCascade.c      	Judy1Cascade.c
copy ..\JudyCommon\JudyCount.c        	Judy1Count.c
copy ..\JudyCommon\JudyCreateBranch.c 	Judy1CreateBranch.c
copy ..\JudyCommon\JudyDecascade.c    	Judy1Decascade.c
copy ..\JudyCommon\JudyDel.c          	Judy1Unset.c
copy ..\JudyCommon\JudyFirst.c        	Judy1First.c
copy ..\JudyCommon\JudyFreeArray.c    	Judy1FreeArray.c
copy ..\JudyCommon\JudyGet.c          	Judy1Test.c
copy ..\JudyCommon\JudyGet.c          	j__udy1Test.c
copy ..\JudyCommon\JudyInsArray.c     	Judy1SetArray.c
copy ..\JudyCommon\JudyIns.c          	Judy1Set.c
copy ..\JudyCommon\JudyInsertBranch.c 	Judy1InsertBranch.c
copy ..\JudyCommon\JudyMallocIF.c     	Judy1MallocIF.c
copy ..\JudyCommon\JudyMemActive.c    	Judy1MemActive.c
copy ..\JudyCommon\JudyMemUsed.c      	Judy1MemUsed.c
copy ..\JudyCommon\JudyPrevNext.c     	Judy1Next.c
copy ..\JudyCommon\JudyPrevNext.c     	Judy1Prev.c
copy ..\JudyCommon\JudyPrevNextEmpty.c	Judy1NextEmpty.c
copy ..\JudyCommon\JudyPrevNextEmpty.c	Judy1PrevEmpty.c
copy ..\JudyCommon\JudyTables.c	        Judy1TablesGen.c


echo "--- This table is constructed from Judy1.h data to match malloc(3) needs"
echo "--- %CC% %COPT%  -I. -I.. -I..\JudyCommon -DJUDY1 Judy1TablesGen.c -o Judy1TablesGen"
%CC% %COPT%  -I. -I.. -I..\JudyCommon -DJUDY1 Judy1TablesGen.c -o Judy1TablesGen
del *.o
.\Judy1TablesGen
echo "--- %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1Tables.c"
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1Tables.c 

echo "--- Compile the main line Judy1 modules"
echo "--- %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1Test.c" 
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1Test.c 
echo "--- %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 -DJUDYGETINLINE j__udy1Test.c"
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 -DJUDYGETINLINE j__udy1Test.c
echo "--- %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1Set.c"
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1Set.c
echo "--- %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1SetArray.c"
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1SetArray.c
echo "--- %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1Unset.c"
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1Unset.c
echo "--- %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1First.c"
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1First.c
echo "--- %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 -DJUDYNEXT Judy1Next.c"
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 -DJUDYNEXT Judy1Next.c
echo "--- %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 -DJUDYPREV Judy1Prev.c"
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 -DJUDYPREV Judy1Prev.c
echo "--- %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 -DJUDYNEXT Judy1NextEmpty.c"
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 -DJUDYNEXT Judy1NextEmpty.c
echo "--- %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 -DJUDYPREV Judy1PrevEmpty.c"
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 -DJUDYPREV Judy1PrevEmpty.c
echo "--- %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1Count.c"
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1Count.c
echo "--- %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 -DNOSMARTJBB -DNOSMARTJBU -DNOSMARTJLB Judy1ByCount.c"
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 -DNOSMARTJBB -DNOSMARTJBU -DNOSMARTJLB Judy1ByCount.c
echo "--- %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1FreeArray.c"
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1FreeArray.c
echo "--- %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1MemUsed.c"
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1MemUsed.c
echo "--- %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1MemActive.c"
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1MemActive.c
echo "--- %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1Cascade.c"
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1Cascade.c
echo "--- %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1Decascade.c"
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1Decascade.c
echo "--- %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1CreateBranch.c"
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1CreateBranch.c
echo "--- %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1InsertBranch.c"
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1InsertBranch.c
echo "--- %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1MallocIF.c"
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1MallocIF.c
echo "--- cd .."
cd ..

echo "--- Give JudyL the proper names"
echo "--- cd JudyL"
cd JudyL
del *.o
copy ..\JudyCommon\JudyByCount.c      	JudyLByCount.c   
copy ..\JudyCommon\JudyCascade.c              JudyLCascade.c
copy ..\JudyCommon\JudyCount.c        	JudyLCount.c
copy ..\JudyCommon\JudyCreateBranch.c 	JudyLCreateBranch.c
copy ..\JudyCommon\JudyDecascade.c    	JudyLDecascade.c
copy ..\JudyCommon\JudyDel.c          	JudyLDel.c
copy ..\JudyCommon\JudyFirst.c        	JudyLFirst.c
copy ..\JudyCommon\JudyFreeArray.c    	JudyLFreeArray.c
copy ..\JudyCommon\JudyGet.c          	JudyLGet.c
copy ..\JudyCommon\JudyGet.c          	j__udyLGet.c
copy ..\JudyCommon\JudyInsArray.c     	JudyLInsArray.c
copy ..\JudyCommon\JudyIns.c          	JudyLIns.c
copy ..\JudyCommon\JudyInsertBranch.c 	JudyLInsertBranch.c
copy ..\JudyCommon\JudyMallocIF.c     	JudyLMallocIF.c
copy ..\JudyCommon\JudyMemActive.c    	JudyLMemActive.c
copy ..\JudyCommon\JudyMemUsed.c      	JudyLMemUsed.c
copy ..\JudyCommon\JudyPrevNext.c     	JudyLNext.c
copy ..\JudyCommon\JudyPrevNext.c     	JudyLPrev.c
copy ..\JudyCommon\JudyPrevNextEmpty.c	JudyLNextEmpty.c
copy ..\JudyCommon\JudyPrevNextEmpty.c	JudyLPrevEmpty.c
copy ..\JudyCommon\JudyTables.c	        JudyLTablesGen.c

echo "--- This table is constructed from JudyL.h data to match malloc(3) needs"
echo "--- %CC% %COPT%  -I. -I.. -I..\JudyCommon -DJUDYL JudyLTablesGen.c -o JudyLTablesGen"
%CC% %COPT%  -I. -I.. -I..\JudyCommon -DJUDYL JudyLTablesGen.c -o JudyLTablesGen
del *.o
.\JudyLTablesGen 
echo "--- %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLTables.c"
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLTables.c 

echo "--- Compile the main line JudyL modules"
echo "--- %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLGet.c"
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLGet.c
echo "--- %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL -DJUDYGETINLINE j__udyLGet.c"
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL -DJUDYGETINLINE j__udyLGet.c
echo "--- %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLIns.c"
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLIns.c
echo "--- %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLIns.c"
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLInsArray.c
echo "--- %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLDel.c"
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLDel.c
echo "--- %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLFirst.c"
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLFirst.c
echo "--- %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL -DJUDYNEXT JudyLNext.c"
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL -DJUDYNEXT JudyLNext.c
echo "--- %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL -DJUDYPREV JudyLPrev.c"
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL -DJUDYPREV JudyLPrev.c
echo "--- %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL -DJUDYNEXT JudyLNextEmpty.c"
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL -DJUDYNEXT JudyLNextEmpty.c
echo "--- %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL -DJUDYPREV JudyLPrevEmpty.c"
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL -DJUDYPREV JudyLPrevEmpty.c
echo "--- %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLCount.c"
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLCount.c
echo "--- %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL -DNOSMARTJBB -DNOSMARTJBU -DNOSMARTJLB JudyLByCount.c"
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL -DNOSMARTJBB -DNOSMARTJBU -DNOSMARTJLB JudyLByCount.c
echo "--- %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLFreeArray.c"
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLFreeArray.c
echo "--- %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLMemUsed.c"
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLMemUsed.c
echo "--- %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLMemActive.c"
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLMemActive.c
echo "--- %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLCascade.c"
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLCascade.c
echo "--- %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLDecascade.c"
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLDecascade.c
echo "--- %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLCreateBranch.c"
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLCreateBranch.c
echo "--- %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLInsertBranch.c"
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLInsertBranch.c
echo "--- %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLMallocIF.c"
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLMallocIF.c
echo "--- cd .."
cd ..

echo "--- Compile the JudySL routine"
echo "--- cd JudySL"
cd JudySL
del *.o
echo "--- %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c JudySL.c"
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c JudySL.c
echo "--- cd .."
cd ..
echo "--- Compile the JudyHS routine"
echo "--- cd JudyHS"
cd JudyHS
del *.o
echo "--- %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c JudyHS.c"
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c JudyHS.c
echo "--- cd .."
cd ..

rem Make a Judy shared library with CPIC='-fPIC' above
rem ld -shared -o libJudy.so Judy*\*.o
rem 
rem  -OR-
rem 
echo "--- Make a Judy static library"
%AR_CALL%
D:\ghc\ghc-6.4.2\bin\ar -r libJudy.a Judy1/j__udy1Test.o Judy1/Judy1ByCount.o Judy1/Judy1Cascade.o Judy1/Judy1Count.o Judy1/Judy1CreateBranch.o Judy1/Judy1Decascade.o Judy1/Judy1First.o Judy1/Judy1FreeArray.o Judy1/Judy1InsertBranch.o Judy1/Judy1MallocIF.o Judy1/Judy1MemActive.o Judy1/Judy1MemUsed.o Judy1/Judy1Next.o Judy1/Judy1NextEmpty.o Judy1/Judy1Prev.o Judy1/Judy1PrevEmpty.o Judy1/Judy1Set.o Judy1/Judy1SetArray.o Judy1/Judy1Tables.o Judy1/Judy1Test.o Judy1/Judy1Unset.o JudyCommon/JudyMalloc.o JudyHS/JudyHS.o JudyL/j__udyLGet.o JudyL/JudyLByCount.o JudyL/JudyLCascade.o JudyL/JudyLCount.o JudyL/JudyLCreateBranch.o JudyL/JudyLDecascade.o JudyL/JudyLDel.o JudyL/JudyLFirst.o JudyL/JudyLFreeArray.o JudyL/JudyLGet.o JudyL/JudyLIns.o JudyL/JudyLInsArray.o JudyL/JudyLInsertBranch.o JudyL/JudyLMallocIF.o JudyL/JudyLMemActive.o JudyL/JudyLMemUsed.o JudyL/JudyLNext.o JudyL/JudyLNextEmpty.o JudyL/JudyLPrev.o JudyL/JudyLPrevEmpty.o JudyL/JudyLTables.o JudySL/JudySL.o

echo "--- Done"
