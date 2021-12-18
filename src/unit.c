#include "kerf.h"

#define TEST_EQUAL(x,y,...) test_equal((x),(y)) //optional: use macro to indicate line # etc

K The_Test_Kerf_Tree_Saver = NULL;
I The_Test_Cases_Count = 0;
I The_Test_Cases_Passed_Count = 0;
I The_Test_Cases_Failed_Count = 0;
I The_Test_Cases_Skipped_Count = 0;
F The_Test_Start_Time = 0;

void test_suite_initialize()
{
  The_Test_Cases_Count = 0;
  The_Test_Cases_Passed_Count = 0;
  The_Test_Cases_Failed_Count = 0;
  The_Test_Cases_Skipped_Count = 0;

  The_Test_Start_Time = clock();
}

void test_suite_finalize()
{
  F elapsed_time = (clock() - The_Test_Start_Time)/CLOCKS_PER_SEC;
 
  F pass_rate = The_Test_Cases_Passed_Count/((F)The_Test_Cases_Count - The_Test_Cases_Skipped_Count);

  fprintf(stderr, "Test pass rate: %.4f, Total: %lld, Pass: %lld, Skip: %lld, Fail: %lld, Time: %fs\n", 
                   pass_rate,
                   The_Test_Cases_Count, 
                   The_Test_Cases_Passed_Count, 
                   The_Test_Cases_Skipped_Count, 
                   The_Test_Cases_Failed_Count, 
                   elapsed_time);

  if(1.0 == pass_rate)fprintf(stderr, "\x1B[0;32mPassed\x1B[0m\n");
  else fprintf(stderr, "\x1B[1;37;41mFailed\x1B[0m\n"); //see http://ascii-table.com/ansi-escape-sequences.php
}

void test_case_buildup()
{
  rd(The_Cache);//free cache before Kerf Tree

  The_Test_Kerf_Tree_Saver = The_Kerf_Tree;
  The_Kerf_Tree = new_map();

  The_Cache = new_map();
}

void test_case_teardown()
{
  rd(The_Cache);//free cache before Kerf Tree

  //optional: per-case memory leak tracking
  rd(The_Kerf_Tree);
  The_Kerf_Tree = The_Test_Kerf_Tree_Saver;

  The_Cache = new_map();
}

void test_case_handle_result(bool ok)
{
  if(0 == (The_Test_Cases_Count%100)) fprintf(stderr, "Testing case %3lld...\n", The_Test_Cases_Count);

  The_Test_Cases_Count += 1;

  if(ok)
  {
    The_Test_Cases_Passed_Count += 1;
  }
  else
  {
    The_Test_Cases_Failed_Count += 1;
  }
}

C test_equal(S a, S b)
{
  if(!strcmp(a,"skip"))
  {
    The_Test_Cases_Count += 1;
    The_Test_Cases_Skipped_Count += 1;
    return true;
  }

  test_case_buildup();
  rd(ex(".Print.stamp_format: null"));

  assert(a != NULL);
  assert(b != NULL);

  K x = ex(a);
  K y = ex(b);

  assert(x != NULL);
  assert(y != NULL);

  C eval = matchC(x, y, true);
  C is_ok = (1 == eval);

  test_case_handle_result(is_ok);

  if(!is_ok)
  {
    fprintf(stderr,"\nFailed. These are not equal:\n");
    fprintf(stderr,"%s , %s\n", a, b);
    fprintf(stderr,"********************************\n");
    show(x); fflush(stdout);
    fprintf(stderr,"--------------------------------\n");
    show(y); fflush(stdout);
    fprintf(stderr,"\n");
  }

  if(x) rd(x);
  if(y) rd(y);

  test_case_teardown();

  return is_ok;
}

void test_allocations()
{
    fprintf(stderr,"\n[DEBUG] Testing for leaked memory...\n");
    if(TEST_TRY_NO_REFERENCE_DECREMENT || TEST_TRY_POOL_SIDESTEP_VIA_MALLOC) return;
    //print leaks to disk instead of cluttering console?
    I living = 0;
    //skip over mmap registries
    ENUM(The_Allocations, 

                      if(!v || !u || !uk)
                      {
                        dd(v)
                        dd(u)
                        dd(uk)
                      }
    
                      if(vi > 0 && uk->t != -LINK)
                      { living++;
                        fprintf(stderr,"ptr:%lld t:%lld n:%lld\n", (I)uk, (I)uk->t, (I)uk->n);
                        show(uk); 
                      })

    if(living > 0)
    {
      fprintf(stderr,"\nleaked memory. living: %lld\n", living);
      The_Debug_OK_Flag = false;

      if(The_Did_Interrupt_Flag)
      {
        fprintf(stderr,"Previous ctrl+c Interrupt detected. Before reporting, please be sure ctrl+c was not the cause of the leak.\n");
      }
    }
    else
    {
      fprintf(stderr,"[DEBUG] Done...\n");
    }

    //di(The_Allocations,0);
    //show(The_Allocations);
}

K test_incorrect_sort_attr(K x)
{
  if(GET_ATTR(x,ATTR_SORTED) && IS_ARRAY(x))
  {
    if(!is_sorted_array(x))
    {
      fprintf(stderr, "Warning: Object not sorted but marked with sorted attribute.\n");
      OFF_ATTR(x,ATTR_SORTED);//so that show() doesn't recurse
      dd(xt);
    }
  }

  return x;
}

I container_insane(K x)
{

  //The ordering of these tests is fairly important
  //Since broken data fails spectacularly in a particular order

  I m = xm;
  I h = xh;
  I n = box_count_K(x);

  if(h<0 || m < 0)
  {
    fprintf(stderr, "space or header size is negative: h:%lld m:%lld\n", h, m);
    return -1;
  }

  if(n < 0)
  {
    fprintf(stderr, "count is negative: %lld\n", n);
    return -1;
  }

  if(IS_NEST(x) && n > 0)
  {
    if(!h)
    {
      fprintf(stderr, "header size is not set: h:%lld\n", h);
      return -1;
    }

    if(h >= m)
    {
      fprintf(stderr, "header size is too big: h:%lld m:%lld\n", h, m);
      return -1;
    }


    if((n+1)*sizeof(K0) > POW2(h))
    {
      fprintf(stderr, "header too small for indices: h:%lld\n", h);
      return -1;
    }

    DO(n, if(k0i(x,i)->t != JUMP && !IS_STRIPED(x))
          {
            fprintf(stderr, "header jump type not set: h:%lld\n", h);
            return -1;
          }
    )

    I firstjump = x0[0].n;
    if(firstjump != 0)
    {
      fprintf(stderr, "first jump is non-zero: %lld.\n",firstjump);
      return -1;
    }

    if(kJ(x,0) != (POW2(h)+(V)x))
    {
      fprintf(stderr, "first jump doesn't work.\n");
      return -1;
    }

    DO(n-1, if(kJ(x,i) >= kJ(x,i+1))
            {
              fprintf(stderr, "jumps are not ordered at i:%lld\n", i);
              return -1;
            }
    )

    DO(n-1, K y = kJ(x,i); if((POW2(ym) + (V)y) != kJ(x,i+1))
            {
              fprintf(stderr, "jumps are not accurate at i:%lld\n", i);
              return -1;
            }
    )

    DO(n, if(!IS_STRIPED(x) && kJ(x,i) != kN(x,i))
            {
              fprintf(stderr, "jump methods are broken\n");
              return -1;
            }
    )

    if((POW2(m)+(V)x) < (V)kJ(x,n-1) )
    {
      fprintf(stderr, "Final jump is out of bounds.\n");
      return -1;
    }

    NEST(x, if(container_insane(v)) return -1)

  }
  
  return 0;
}

I di(K x, I verbose)
{
  if(!x)
  {
    fprintf(stderr, "X is NULL\n");
    return -1;
  }

  if(verbose)
  {
    fprintf(stderr, "X:%lld checking...\n", (I)x);
    I n = box_count_K(x);

    if(LIST==xt && n!=ECOUNT(x))
    {
      er(bad box)
      return -1;
    }

    fprintf(stderr, "Type: %d Boxes: %lld Ecount: %lld\n", xt, n, ECOUNT(x));
  }

  if(xm <= 0)
  {
    fprintf(stderr, "m is zero or negative: %d\n", xm);
    return -1;
  }

  if(IS_DISK(x))
  {
    if(container_insane(x))
    {
      fprintf(stderr, "Failed disk container sanity check\n");
      return -1;
    }

  }

  if(IS_TABLE(x))
  {
    if(table_ragged(x))
    {
      fprintf(stderr, "Failed table ragged sanity check\n");
      return -1;
    }

  }

  if(GET_ATTR(x,ATTR_SORTED) && IS_ARRAY(x) && !is_sorted_array(x))
  {
    dd(x);
    dd(xt)
    dd(lenI(x))
    //Don't call show() here if show calls this
    fprintf(stderr, "Failed sort check\n");
    return -1;
  }

  if(verbose)
  {
    fprintf(stderr, "X:%lld Pass\n", (I)x);
  }

  return 0;
}

  //charvec = kcv("1+1+-1 -alpha + 123.0E+0  \"ok\" - `'cool'");
  //charvec = kcv("2014.11.25T12:23:45.00 2013.01.01  1:34  ` \"fun\" `'aaa' ");
  //charvec = kcv("2013y10m2d 2013y.24M 3m2d");
  //charvec = kcv(" 2012.01.01 2013.01.01");
  //charvec = kcv("+\\/ fold -1 1-1 a b minus if  1 + nil-1");
  //charvec = kcv("select a from b");
  //charvec = kcv("1 plus select a from b");
  //charvec = kcv("select a from b where c = d, q < f; select a from b"); //charvec = kcv("select a from b where c = d, q < f ");
  //charvec = kcv("(select a from b where c = d, q < f )");
  //charvec = kcv("(select a AS a1,b,c from d,e,f group by x,y where g = h, j < k )");
  //charvec = kcv("select a from (select b from c)");
  //charvec = kcv("1+1; 1+(2+3)+(4+5)+6 ; 3+3");
  //charvec = kcv("1+()+3");
  //charvec = kcv("1+2+3 + fold \"funtimes\" ");
  //charvec = kcv("Return a [1;2] +: {b:1, c:2}//this is a comment");

  //play(" (3/10) + 1.5");
  //play(" \"super fun\" ");
  //play(" 'super fun'");
  //play(" '\\u0065'  ");
  //play(" '\\\\'  ");

  //play("self");
  //play(".sess.k");
  //play("k");
  //play(".k");
  //play("plus(1, 2, 3)");
  //play("f(1, 2)");

  //play("join fold reconverge [[[0,1],[5,6]],[2,3]] //TODO doesn't work right");
  //play("[0,1,2,3,4]");

  //play("[[0,1],[2,3]]");

  //play("{[x,y,z] 1+x} //function");

  //play("{[x].sess} 0");
  //play("{[x].sess.a} 0");
  //play(".sess");
  //play(".sess.a");

  //play("{[a] 1 + a}");
  //play("     1+");
  //play("1+");
  //play("[1+, 2+]");
  //play("(1,2,3,4,5,6) 0");
  //play("([1+,1+])");
  //play("+");
  //play("(+)");
  //play("1+; 2+");

  //play("[1, 2, 3]");

  //play("{[] .sess } 0");
  //play("{[].sess.a} 0");
  //play("{[];1;;;}    ");

  //play("{{col:1 2}}   //table");

  //K a = til(ki(1000000));
  //K b = ki(0);
  //TIME(DO(COUNT(a), c += kI(a)[i]))
  //TIME( DO(1, play("+ fold a;")))
  //TIME( DO(1, play("+ refold a;")))
  //TIME(ENUM(a, K t=b; b=plus(v,b); rd(t)))
  //TIME(DO(COUNT(a), K t=b; b=plus(ki(kI(a)[i]),b); rd(t)))

  //play("cool(1,2)");
  //play("cool fold 1 2 3");

  ///////////////////////////////////////////////////////
  //Error Catching / Intentional Errors
  //play("(+) 1 //error");
  ///////////////////////////////////////////////////////
  //Brokenness / Unintentional
  //play("."); //broken or not?
  //play("+");
  //play("a[0] :  5"); //broken: if a doesn't exist this should fail?
  ///////////////////////////////////////////////////////

  //play("= 1 2 3");
  //play("# 1 2 3");
  //play("indexed 1 2 3");
  //play("hashed 1 2 3");
  //play("btree 1 2 3");
  //play("hash 1 2 3");
  //play("{{a:HASHED 1 2 3}}");
  //play("b:1 2 3; {{a:INDEXED b}}");

  //show(cow_table_insert(interpret(kcv("{{f:0 0}}")),interpret(kcv("[\"f\"]")),ki(1)));

  //play("t:{{a, b}}");
  //play("t:{{a:111,b:2015.02.03T14:05:00}}");
  //play("t:{{a:111,b:33}}");

  //TIME(DO(1, play("t:{{a:HASHED[],b:HASHED[]}}; insert into t values [til 1000000, til 1000000];");))
  //TIME(DO(1, play("t:{{a:INDEXED[],b:INDEXED[]}}; insert into t values [til 1000, til 1000];");))
  //TIME(DO(1, play("t:{{a,b}}; insert into t values [til 1000000, til 1000000];");))
  //play("insert into {{a:0, b:0}} values (111,222)");
  //play("{{a:111, b:222}}");
  //play("insert into {{a, b}} values (111,222)");
  //play("insert into t (a, b) values (123, 2015.02.03T14:06:06); t");
  //play("INSERT INTO t (a, b) VALUES (789, 2015.02.03T14:07:07); t");
  //play("INSERT INTO t (a, b) VALUES (999, 'War & Peace'); t");



  //play("select a,b,c from t,u,v where q,r,s order by x,y,z");
  //play("select max(quantity) from t");
  //play("//NOT YET update t set a=1+c, b=2+d where q,r,s");

  //K k = ki(0);
  ////k = ex("{[a,b] [a,b]}", ki(11), ki(33));
  ////k = ex("{[a,b] a-b}", ki(11), ki(33));
  ////k = ex("f:{[a,b,c] [a,b,c]}; f(1,2,3)");
  ////k = ex("11-33");
  ////k = ex("1+", ki(11));
  ////k = ex("+", ki(11), ki(22));
  ////k = ex("-", ki(11), ki(22));
  ////k = ex("1+1");


  //play("a:{b:1,c:{d:2}};//maps"); play("[ a.c, a['c']]");
  //play("a:{{b:1,c:2}} //tables"); play("[ a.c, a['c']]");


  //play("4+[]");

  //play("range(4,3)");

  //play("_ 1.2 4.5 6.7");
  //play("_ 1 4 6");
  //play("_ 'Super Happy FUNtime'");
  //play("floor 2.2 6.5 6.7");



  //play("grouped 9 8 7 6 5 8 8 7 9 5 3 5 7 6 3");
  //play(" .'1+1'");

  //Full range of correct stack order tests:
  //(there should be easy toggles for all of this so you can get it all to fit together)
  //{verbs, functions, derived verbs, nouns (deep lists)} X
  //{adverbs, juxtaposition, round index, square index} X
  //{_ex method, }
  //
  // 

  //These are worrisome:
  //generally, empty list indexing into should return same empty list
  //in case of a table...probably return table with no rows?
  //play("t 0 take 0");
  //play("t 0");
  //play("t [0]");
  //play("t []");
  //play("1 2 3 []");
  //play("{[x] x + 1} 3");
  //play("{[a]a take [[]]} 3");
  //play("3 take [[]]");

  //K t = ex("n: 10**1; r: range(n); t:{{a:INDEXED 101+r, b:4*(1+r), c:n take [2,2,3], d: n^4 5 5}}");
  //rd(t);
  //K t = ex("n: range(5); t:{{a:INDEXED 101+n, b:4*(1+n), c:a+a}}");

  //K t = Kk();
  //K t = ex("n: range(5); t:{{a: INDEXED 33 00 33 22 33 11}}");
  //play("select from t");
  //play("select from t where 103 <  a");
  //play("select from t where 103 <= a");
  //play("select from t where a   <= 103");
  //play("select from t where 103 >  a");
  //play("select from t where a   >  103");
  //play("select from t where 103 >= a");
  //play("select from t where a   >= 103");
  //play("select from t where a   == 103");
  //play("select from t where a   != 103");

  //play("u: select a,c from t where(a > 102), c >= 208 group by b limit 10"); 
  //play("u");
  //play("< u");

  //play("select from t where a < 110 group by c, d");
  
  //play("FIRST 1 2 3");
  //play("*,*,(*),count(*)");

  //play("select * from t");
  //play("select *,( * ) from t");
  //play("select count( * ) + 1 from t");

  //play("select a as A, b as B from t where a < 110");
  //play("select a, b, c from t where a < 110");
  //play("select a, b, b from t where a < 110");
  //play("select b, b, b as cat from t where a < 110");
  //play("select a, b, b, b from t where a < 110");
  //play("select a, b, b, b, b, b from t where a < 110 //works");
  //play("select a as a, b as b, b as b1, b as b2 from t where a < 110");
  //play("select a, b as xyz, c from t where a < 110");
  //play("select a as a, b as b from t group by c where a < 110");
  //play("select a as a, (select b as bsub from t) as ignored_as_expected from t");

  //play("select a as a, b as b from t group by c where a < 110");

  //play("select from t where a = -1");
  //play("first select from t where a = -1");
  //play("first select from t where a < 110");

  //play("select sum(a),1  from t where a < 110");
  //play("select sum(a),1 2 from t where a < 110");
  //play("select sum(a),1 2 from t where a = 3");

  //play("select sum(a) from t where a < 110");
  //play("select min(a) from t where a < 110");
  //play("select max(a) from t where a < 110");

  //play("select from t where a < 110");
  //play("select sum(a) from t group by c where a < 110");
  //play("select a from t group by c where a < 110");
  //play("select [], unique(a), sum(a) from t group by c where a < 110");

  //play("select sum(a) from t where a < 110");
  //play("select min(a) from t where a < 110");
  //play("select max(a) from t where a < 110");

  //play("select from t where a < 110");
  //play("select sum(a) from t group by c where a < 110");
  //play("select a from t group by c where a < 110");
  //play("select [], unique(a), sum(a) from t group by c where a < 110");
  //play("select sum(a), count(a), first(a), last(a), avg(a), std(a), var(a) from t group by c where a < 110");
  //play("select sum(a), count(a), first(a), last(a), avg(a), std(a), var(a) from t group by c,d where a < 120");
  //play("select std(a)    from t group by c where a < 110");
  //play("select var(a)    from t group by c where a < 110");
  //play("select unique(a) from t group by c where a < 110");

  //play("10 ? 3");
  //play("-4 ? 4");
  //play("4 ? 'summerhome'");
  //play("-6 ? 'quince'");
  //play("5 ? 1.0");
  //play("5 ? 0");
  //play("5 ? 0.0");
  //play("5 ? 1 2 3 4");
  //play("-4 ? 1 2 3 4");
  //play("2 ? {a:1,b:2,c:3}");
  //play("3 ? null");
  //play("-3 ? null");

void run_tests()
{
  test_suite_initialize();

  TEST_EQUAL("1+1","2");
  TEST_EQUAL("11 22 33 44", "11 * 1 + range 4");
  TEST_EQUAL("1-1","0");
  TEST_EQUAL("1 plus 1","2");
  TEST_EQUAL("2", "a:1; a+a");
  TEST_EQUAL("2", "1 plus fold 1");
  TEST_EQUAL("3 3", "1 + 2 2");
  TEST_EQUAL("1+  ; 0","0");//testing parsing, sanity
  TEST_EQUAL("(1+); 0","0");
  TEST_EQUAL("f:1+; 0","0");
  TEST_EQUAL("f:1 ; 0","0");
  TEST_EQUAL("f:(1+);0","0");
  TEST_EQUAL("f:(1+); f 2","3");
  TEST_EQUAL("f:1; f+:2","3");
  TEST_EQUAL("floor 100 * std [9, 2, 5, 4, 12, 7, 8, 11, 9, 3, 7, 4, 12, 5, 4, 10, 9, 6, 9, 4]"," 298");
  TEST_EQUAL("floor 100 * sqrt 2","141");
  TEST_EQUAL("3 repeat 'fun'","['fun','fun','fun']");
  TEST_EQUAL("2015.01.21T12:34:56.123['hour']","12");
  TEST_EQUAL("2015.01.21T12:34:56['second']","56");
  TEST_EQUAL("2015.01.21['day']","21");
  TEST_EQUAL("12:34:56.123['millisecond']","123");
  TEST_EQUAL("2015Y+0D","2015Y0D");
  TEST_EQUAL("2015Y12M","2015y12m");
  TEST_EQUAL("2015Y-12M","2015y-12m");
  TEST_EQUAL("2015Y.12M","2015y12m");
  TEST_EQUAL("00:00:09.100['second']","9");
  TEST_EQUAL("00:00:09.101['hour']","0");
  TEST_EQUAL("00:00:09.0101['millisecond']","10");
  TEST_EQUAL("00:00:09.00101['microsecond']","1010");
  TEST_EQUAL("00:00:09.000101['microsecond']","101");
  TEST_EQUAL("00:00:09.000['nanosecond']","0");
  TEST_EQUAL("00:00:04.999999['nanosecond']","999999000");
  TEST_EQUAL("([[]])","[[]]");
  TEST_EQUAL("1+(2+3)+4","10");
  TEST_EQUAL("","null");
  TEST_EQUAL("{}","map([],[])");
  TEST_EQUAL("reverse 1 2","2 1");
  TEST_EQUAL("negate reverse range 2","-1 0");
  TEST_EQUAL("1 plus 4 4 4 4","5 5 5 5");
  TEST_EQUAL("1 plus 1","2");
  TEST_EQUAL("1+1; plus fold range 4","6");
  TEST_EQUAL("(2+3)","5");
  TEST_EQUAL("(2+3+4)","9");
  TEST_EQUAL("1+(2+3+4)","10");
  TEST_EQUAL("(2+3+4) + 5","14");
  TEST_EQUAL("1+(2+(3+4))", "10");
  TEST_EQUAL("[2,12,6]", "[1+1; 1+(2+3)+6 ; 3+3]");
  TEST_EQUAL("[1, 12 22 32]","a:1 2; a[1] +: 10 20 30");
  TEST_EQUAL("18","1+(2+(3+4)+5)+3");
  TEST_EQUAL("[1.0, 1.0, 0.1, 1.0, 10.0, nan, inf, nan]","1 1. .1 1.0 1.0e+1 nan inf NaN");
  TEST_EQUAL("7","def fun(a,x){a+x}; fun(3,4)");
  TEST_EQUAL("{{a: 1 2, b:8 10}}","select sum b from {{a: 1 2 1 2, b: 3 4 5 6}} group by a");
  TEST_EQUAL("{{a:['c','d'], b:8 10}}","select sum b from {{a: ['c','d','c','d'], b: 3 4 5 6}} group by a");
  TEST_EQUAL("{{a:#['c','d'], b:8 10}}","select sum b from {{a: #['c','d','c','d'], b: 3 4 5 6}} group by a");
  TEST_EQUAL("{{a:#['c','d'], b:2 2}}","select count b from {{a: #['c','d','c','d'], b: 3 4 5 6}} group by a");
  TEST_EQUAL("{{a:#['c','d'], b:4.0 5.0}}","select avg b from {{a: #['c','d','c','d'], b: 3 4 5 6}} group by a");
  TEST_EQUAL("{{a:#['c','d'], b:5 6}}","select last b from {{a: #['c','d','c','d'], b: 3 4 5 6}} group by a");
  TEST_EQUAL("{{a: 1 2 2, c: 2 1 2,  b:8 4 6}}","select sum b from {{a: 1 2 1 2, c: 2 1 2 2, b: 3 4 5 6}} group by a,c");
  TEST_EQUAL("{{a:['c','d','d'],c:['d','c','d'], b:8 4 6}}","select sum b from {{a: ['c','d','c','d'],c:['d','c','d','d'], b: 3 4 5 6}} group by a,c");
  TEST_EQUAL("true","'FUN'  < 'fun'"); // b versus b b b b
  TEST_EQUAL("true","'fun'  < 'FUNK'");
  TEST_EQUAL("true","'FUNK' < 'Funk'");
  TEST_EQUAL("true","'funk' < 'GUNK'");
  TEST_EQUAL("3 2 1","reverse(1 2 3)");
  TEST_EQUAL("8","plus fold 1 3 4");
  TEST_EQUAL("flatten 1","1");
  TEST_EQUAL("flatten [1]","[1]");
  TEST_EQUAL("flatten [[1]]","[1]");
  TEST_EQUAL("join fold [[1]]","[1]");
  TEST_EQUAL("# fold [[1]]","[1]");
  TEST_EQUAL("flatten []","[]");
  TEST_EQUAL("flatten [[]]","[]");
  TEST_EQUAL("aa:3 1 6 2 6 4 6 3; {{a:aa, c: NAN 41 78 20 78 NAN 78 NAN}}","left_join({{a:aa}},{{a:6 0 7 5 2 1 1 6, c: 78 28 20 76 20 41 67 61}}, ['a'])");
  TEST_EQUAL("aa: 1 4 2 2 0; bb: 0 2 4 1 0; cc:1 1 2 2 3; dd: 33 22 22 11 11;  left_join({{a:aa, b:bb, c:cc}}, {{a:reverse aa, b:reverse bb, d:dd}}, ['a','b'])","{{a:aa,b:bb,c:cc,d:reverse dd}}");
  TEST_EQUAL("nan 1 1 2 8.5 4","trades: {{time: 07:00 08:30 09:59 10:00 12:00 16:00, sym:enum['a','a','a','a','b','a'], price: .9 1.5 1.9 2 9 10, size: 100 700 200 400 500 800}}; quotes: {{time: 08:00 09:00 10:00 11:00 12:00 13:00 14:00 15:00, sym:enum['a','b','a','b','b','a','b','a'], bid: 1 9 2 8 8.5 3 7 4}}; j:asof_join(trades, quotes, ['sym'], ['time']); j['bid']");
  TEST_EQUAL("[2, 8, 0, 5, 1, 6, 4, 7, 3]","< 1 3 0 7 5 2 4 6 0");
  TEST_EQUAL("[3, 7, 4, 6, 1, 5, 0, 2, 8]","> 1 3 0 7 5 2 4 6 0");
  TEST_EQUAL("[2, 8, 0, 5, 1, 6, 4, 7, 3]","< 1 3 0 7 5 2 4 6 0.0");
  TEST_EQUAL("[3, 7, 4, 6, 1, 5, 0, 2, 8]","> 1 3 0 7 5 2 4 6 0.0");
  TEST_EQUAL("'gimnrsty'","a:'mystring'; a[<a]");
  TEST_EQUAL("'ytsrnmig'","a:'mystring'; a[>a]");
  TEST_EQUAL("< [83.0472, 300.291, 407.556, 880.774, 229.121]","0 4 1 2 3");
  TEST_EQUAL("a:[-2, 1, -2e+07, nan, 0, -inf, 2e+07, 10, -10, -inf, -1, nan, -1e+08, -1e+09, inf, 1e+08, 2, inf];a[<a]","nan nan -inf -inf -1e9 -1e8 -2e7 -10 -2 -1 0 1 2 10 2e7 1e8 inf inf");
  TEST_EQUAL("{{a:6}}","select sum(a) from {{a:1 2 3 4 5}} where a < 4");
  TEST_EQUAL("t:{{a:1 2,b:3 4,c:5 6}}; delete from t","'t'");
  TEST_EQUAL("t:{{a:1 2,b:3 4,c:5 6}}; delete from t; t","{{a:INT[],b:INT[],c:INT[]}}");
  TEST_EQUAL("delete from {{a:1 2,b:3 4,c:5 6}} where a > 1", "{{a:1,b:3,c:5}}");
  TEST_EQUAL("t:{{a:1 2,b:3 4,c:5 6}}; delete from t where a > 1",   "'t'");
  TEST_EQUAL("t:{{a:1 2,b:3 4,c:5 6}}; delete from t where a > 1; t","{{a:1,b:3,c:5}}");
  TEST_EQUAL("t:{{a:1 2 3,b:3 4 5,c:5 6 7}}; delete from t where a >= 1, b >= 4; t","{{a:1,b:3,c:5}}");
  TEST_EQUAL("insert into {{}} values {a:1,b:2}","{{a:1,b:2}}");
  TEST_EQUAL("t:{{}}; insert into t values {a:1,b:2}","'t'");
  TEST_EQUAL("t:{{}}; insert into t values {a:1,b:2}; t","{{a:1,b:2}}");
  TEST_EQUAL("insert into ({{a:1}} + {{a:1}}) values {a:1}", "{{a:2 1}}");
  TEST_EQUAL("insert into ({{a:1,b:2}} + {{a:1,b:2}}) values {a:1,b:2}", "{{a:2 1,b:4 2}}");
  TEST_EQUAL("n:20; t:{{a:range(n), b:100+range(n)}}; delete from t where a = 6; delete from t where a < 10; first t['a']","10");
  TEST_EQUAL("min 2015.01.02 2014.03.04","2014.03.04");
  TEST_EQUAL("max 2015.01.02 2014.03.04","2015.01.02");
  TEST_EQUAL("select from {{a,b,c}} where 0 group by c","{{a,b,c}}");
  TEST_EQUAL("rsum range 4","0 1 3 6");
  TEST_EQUAL("3 msum range 10","[0, 1, 3, 6, 9, 12, 15, 18, 21, 24]");
  TEST_EQUAL("f:{[x,y,z]x+y+z}\\\\; f(1,1 2 3,10 20 30)","[12, 34, 67]");
  TEST_EQUAL("3 take enum take(0,1 1)","enum[NAN, NAN, NAN]");//type preserving
  TEST_EQUAL("0 take enum[]","enum[]");
  TEST_EQUAL("3 take {{a:take(0,[1]),b:take(0,[1])}}","{{a:[NAN,NAN,NAN],b:[NAN,NAN,NAN]}}");
  TEST_EQUAL("3 take {{a,b}}","{{a:[null,null,null],b:[null,null,null]}}");
  TEST_EQUAL("0 take {{a,b}}","{{a,b}}");
  TEST_EQUAL("3 mavg 1 2 NAN 3 4 5","[1, 1.5, 1.5, 2.5, 3.5, 4.0]");
  TEST_EQUAL("'abcdef'   < char('c') ","1 1 0 0 0 0");
  TEST_EQUAL("'abcdef'  <= char('c') ","1 1 1 0 0 0");
  TEST_EQUAL("'abcdef'  >= char('d') ","0 0 0 1 1 1");
  TEST_EQUAL("char('b')  = 'abcdef'","0 1 0 0 0 0");
  TEST_EQUAL("char('b') == 'abcdef'","0 1 0 0 0 0");
  TEST_EQUAL("char('b') <= 'abcdef'","0 1 1 1 1 1");
  TEST_EQUAL("char('b') >= 'abcdef'","1 1 0 0 0 0");
  TEST_EQUAL("select from {{a:10 20 30 40}} where char('b') <= 'aabc' ","{{a:30 40}}");
  TEST_EQUAL("select from {{a:10 20 30 40}} where 'aabc' > char('b') ","{{a:40}}");
  TEST_EQUAL("cdr 1 2 3","2 3");
  TEST_EQUAL("  | mapback []",    "[]");
  TEST_EQUAL("  | mapback 0",     "0"); //could return list instead. side effect of maxback. (was [0], questionably, prior to maxback)
  TEST_EQUAL("  | mapback 0 0",   "0 0");
  TEST_EQUAL("  | mapback 0 0 0", "0 0 0");
  TEST_EQUAL("1 | mapback 0 0 0", "1 0 0");
  TEST_EQUAL("3 mmax 2 0 0 1 0 1 0 1 0","[2, 2, 2, 1, 1, 1, 1, 1, 1]");
  TEST_EQUAL("| mapback [1 2 , 3 4, 3 3 ]", "[[1, 2], [3, 4], [3, 4]]");
  TEST_EQUAL("& mapback [1 2 , 3 4, 3 3 ]", "[[1, 2], [1, 2], [3, 3]]");
  TEST_EQUAL("int 1.1 2.2 3.3","floor 1 2 3");
  TEST_EQUAL("float int 1 2 3","(1 2 3)/1.0");
  TEST_EQUAL("< #[1, 2, 3]","[0, 1, 2]");
  TEST_EQUAL("< #[3, 1, 2]","[1, 2, 0]");
  TEST_EQUAL("> #[1, 2, 2, 3]","3 1 2 0");
  TEST_EQUAL("> #[3, 1, 2, 2]","1 0 2 3");
  TEST_EQUAL("< #['C', 'DDDD', 'A', 'B']","2 3 0 1");
  TEST_EQUAL("a: 22.0 nan 11.0 -11.0 -33.0 0.0 -22.0 ; a[<a]","nan -33.0 -22.0 -11.0 0.0 11.0 22.0");
  TEST_EQUAL("a: NAN 11 -11 0; a[<a]","NAN -11 0 11");
  TEST_EQUAL("a: 22 NAN 11 -11 0 -22 ; a[<a]","NAN -22 -11 0 11 22");
  TEST_EQUAL("a: NAN 11 0; a[<a]","NAN 0 11");
  TEST_EQUAL("xkeys part 10 20 0 0 0 0 30 40 11 10","10 20 0 30 40 11");
  TEST_EQUAL("distinct #20 20 10 10 90", "20 10 90");
  TEST_EQUAL("isnull(NAN, 4)","4");
  TEST_EQUAL("ifnull(nan, 10)","10");
  TEST_EQUAL("isnull(5, 4)",  "5");
  TEST_EQUAL("isnull([12, 13, NAN, 15], 3)","12 13 3 15");
  TEST_EQUAL("isnull([12, 13, NAN, 15])","0 0 1 0");
  TEST_EQUAL("2015.01.02 | 2016.01.02","2016.01.02");
  TEST_EQUAL("2015.01.02 | 2014.01.02","2015.01.02");
  TEST_EQUAL("'cat'|'dog'","'dog'");
  TEST_EQUAL("'dog'|'cat'","'dog'");
  TEST_EQUAL("char('b')|'cat'","'cbt'");
  TEST_EQUAL("'cat'|char('b')","'cbt'");
  TEST_EQUAL("2015.01.02 & 2016.01.02","2015.01.02");
  TEST_EQUAL("2015.01.02 & 2014.01.02","2014.01.02");
  TEST_EQUAL("'cat'&'dog'","'cat'");
  TEST_EQUAL("'dog'&'cat'","'cat'");
  TEST_EQUAL("char('b')&'cat'","'bab'");
  TEST_EQUAL("'cat'&char('b')","'bab'");
  TEST_EQUAL("max(['Pear', 'Apple', 'Orange'])","'Pear'");
  TEST_EQUAL("min(['Pear', 'Apple', 'Orange'])","'Apple'");
  TEST_EQUAL("max hash (['Pear', 'Apple', 'Orange'])","'Pear'");
  TEST_EQUAL("min hash (['Pear', 'Apple', 'Orange'])","'Apple'");
  TEST_EQUAL("max index (['Pear', 'Apple', 'Orange'])","'Pear'");
  TEST_EQUAL("min index (['Pear', 'Apple', 'Orange'])","'Apple'");
  TEST_EQUAL("max hash take(0, 1 2)","-INF");
  TEST_EQUAL("max hash take(0, 1 2.0)","-inf");
  TEST_EQUAL("max index take(0, 1 2)","-INF");
  TEST_EQUAL("max index take(0, 1 2.0)","-inf");
  TEST_EQUAL("repeat( 0,999)","INT[]");
  TEST_EQUAL("repeat(-1,999)","INT[]");
  TEST_EQUAL("repeat( 1,999)","[999]");
  TEST_EQUAL("shift( 1, 10 20 30 40)",    "[NAN, 10, 20, 30]");
  TEST_EQUAL("shift(-1, 10 20 30 40)",    "[20, 30, 40, NAN]");
  TEST_EQUAL("shift(1, 10 20 30 40, 99)", "[99, 10, 20, 30]");
  TEST_EQUAL("shift(-1, 10 20 30 40, 99)","[20, 30, 40, 99]");
  TEST_EQUAL("{[a] if (a == 5) { return 999 }; return 0} 5","999");
  TEST_EQUAL("{[a] if (a == 5) { 0 } else {999} } 5","0");
  TEST_EQUAL("{[a] if (a == 5) { 0 } else {999} } 0","999");
  TEST_EQUAL("{[a] if (a == 5) { return 999 }; return 0} 0","0");
  TEST_EQUAL("{[] if(1){if(1){return 2}}; return 3} 0","2");
  TEST_EQUAL("{[a] if (not len a) { return [] }; return join(car a, enlist self(cdr a))} [1,2,3]","[1,[2,[3,[]]]]");
  TEST_EQUAL("hash(['A','B','A']) in  ['C','C','A'] ","1 0 1");
  TEST_EQUAL("hash(['A','B','A']) in #['C','B','D'] ","0 1 0");
  TEST_EQUAL("intersect(1 1 1 2 2 2 1 1 1 1, 0 0 0 0)","INT[]");
  TEST_EQUAL("intersect(1 1 1 2 2 2 1 1 1 1, 0 0 0 0 1 2)","1 2");
  TEST_EQUAL("intersect(1 1 1 2 2 2 1 1 1 1, 0 0 1 0 0)","[1]");
  TEST_EQUAL("intersect(2, 0 0 0 0 2)","[2]");
  TEST_EQUAL("a:#['A1','B2','C3','D4' ]; b: a[0 1]; a in b","1 1 0 0"); //clean hashset
  TEST_EQUAL("t:{{a:#['A1','B2','C3','D4']}}; delete from t where a <= 'B2'; t['a']","#['C3','D4']");
  TEST_EQUAL("t:{{a:index['A1','B2','C3','D4']}}; delete from t where a <= 'B2'; t['a']","index ['C3','D4']");
  TEST_EQUAL("t:{{a:['A1','B2','C3','D4']}}; delete from t where a <= 'B2'; t['a']","['C3','D4']");
  TEST_EQUAL("t:{{a:['A1','B2','C3','D4']}}; delete from t where a >= 'A1'; t['a']","[]");
  TEST_EQUAL("a:[1 2, 3 4]; b:[5 6, 7 8]; a mmul b","[19 22, 43 50]");
  TEST_EQUAL("a:[1 2 3, 4 5 6]; b:[7 8, 9 10, 11 12]; a mmul b","[58 64, 139 154]");
  TEST_EQUAL("a: [6 2, 2 3]; b: 22 10; sum(abs minus(lsq(a,b), [23/7, 8/7])) < pow(10, -9)","1");
  TEST_EQUAL("a: [6 2, 2 3]; b: 22 10; sum(abs minus(    a\\b, [23/7, 8/7])) < pow(10, -9)","1");
  TEST_EQUAL("pow(10, -9) > sum fold minv([1 2, 3 4]) - [-2 1, 1.5 -0.5]", "1");
  TEST_EQUAL("1 fold 112 113 114","3 take NAN");
  TEST_EQUAL("2016.01.01 mapright 115 116 117","3 take null");
  TEST_EQUAL("({[x,y] x|y} mapback) 118 2 3 119","118 118 3 119");
  TEST_EQUAL("201 202 203 204 mapright 3 0 1 2", "[204, 201, 202, 203]");
  TEST_EQUAL("names: ['KM', 'CM', 'SM', 'TM']; places: ['USA', 'CAN', 'USA', 'UK']; livesin: {{name: names, nationality: places}}; delete from livesin where name == 'SM'; livesin","{{name: ['KM', 'CM', 'TM'], nationality: ['USA', 'CAN', 'UK']}}");
  TEST_EQUAL("names: ['KM', 'CM', 'SM', 'TM']; places: ['USA', 'CAN', 'USA', 'UK']; livesin: {{name: names, nationality: places}}; delete from livesin where name >= 'SM'; livesin","{{name: ['KM', 'CM'], nationality: ['USA', 'CAN']}}");
  TEST_EQUAL("names: ['KM', 'CM', 'SM', 'TM']; places: ['USA', 'CAN', 'USA', 'UK']; livesin: {{name: names, nationality: places}}; delete from livesin where name <= 'KM'; livesin","{{name: ['SM', 'TM'], nationality: ['USA', 'UK']}}");
  TEST_EQUAL("names: ['KM', 'CM', 'SM', 'TM']; places: ['USA', 'CAN', 'USA', 'UK']; livesin: {{name: names, nationality: index places}}; delete from livesin where name <= 'KM'; livesin","{{name: ['SM', 'TM'], nationality: index ['USA', 'UK']}}");
  TEST_EQUAL("q:205; select q as d from {{a,b,c}}","{{d:205}}");
  TEST_EQUAL("g:{[]q:206; select q as d from {{a,b,c}}}; g()", "{{d:206}}"); //locals visible to sql lambdas
  TEST_EQUAL("index([10, 20, 30, 40, 50, 60]) between 20 40","[0, 1, 1, 1, 0, 0]");
  TEST_EQUAL("index([10, 20, 30, 40, 50, 60.0]) between 20 40","[0, 1, 1, 1, 0, 0]");
  TEST_EQUAL("index([10, 20, 30, 40, 50, 60.0]) <  INT(40)",    "[1, 1, 1, 0, 0, 0]");
  TEST_EQUAL("index([10, 20, 30, 40, 50, 60.0]) <= FLOAT(40.0)","[1, 1, 1, 1, 0, 0]");
  TEST_EQUAL("{[a,b] if (a > 0) { this(a-1, b+1) } else { b }}(3, 5)","8");
  TEST_EQUAL("{[a] if (a[0] > 0) { self [a[0]-1,a[1]+1] } else { a[1] }} [2, 3]","5");
  TEST_EQUAL("a:207; a",            "207");
  TEST_EQUAL(".user.a:208; .user.a","208");
  TEST_EQUAL(".a:209; .a",          "209");
  TEST_EQUAL(".a.b:210; .a.b",      "210");
  TEST_EQUAL("a:211; .user.a",      "211");
  TEST_EQUAL("{[].user.a}0;212",    "212");//don't error
  TEST_EQUAL("a:1.1 2.2 3.3; a[1]:213; kerf_type a","-3");
  TEST_EQUAL("a:1 2 3; a[1]:214.0; kerf_type a",    "-3");
  TEST_EQUAL("a:1.1 2.2 3.3; a[1]:215; a", "1.1 215.0 3.3");
  TEST_EQUAL("a:1 2 3; a[1]:216.2; a",     "1.0 216.2 3.0");
  TEST_EQUAL("update {{a:220 221 222}} set","{{a:220 221 222}}");
  TEST_EQUAL("update {{a:217 218 219}} set a=3 3 219, 'b'=1 + 4 5 6, c=,,d=2,e:2 where a>=217","{{a:3 3 219, b:5 6 7, c:[null, null, null], d:2 2 2, e: 2 2 2}}");
  TEST_EQUAL("update {{a:220 221 222}} set b:2*a","{{a:220 221 222, b:440 442 444}}");
  TEST_EQUAL("update {{a:223 224 225}} set a=4 4 225, 'b':2*a, c=99","{{a:4 4 225, b:8 8 450, c: 99 99 99}}");
  TEST_EQUAL("update {{a:1 2 226}} set b=2*a, c=2*b","{{a: 1 2 226, b:2 4 452, c: 4 8 904}}");
  TEST_EQUAL("update {{a:1 2 226}} set b=2*a, c=2*b where a <= 226","{{a: 1 2 226, b:2 4 452, c: 4 8 904}}");
  TEST_EQUAL("update {{a:10 9 227}} set a={{b:3 4 228}}","{{a:3 4 228}}");
  TEST_EQUAL("t:{{a:11 12 13}}; update t set a={{b:3 4 229}}; t ","{{a:3 4 229}}");
  TEST_EQUAL("t:{{a:11 12 13}}; update t set b={{c:3 4 229.1}}; t ","{{a:11 12 13, b:3 4 229.1}}");
  TEST_EQUAL("update {{a:1 2 230 2, b:3 3 1 3, c: 4 4 230 4}} set c=a+sum(b) group by a,b where b > 1"," {{a:1 2 230 2, b: 3 3 1 3, c: 4 8 230 8}} ");
  TEST_EQUAL("update {{a:1 2 231 2, b:3 3 1 3, c: 4 4 231 4}} set c=a+sum(b), d=c group by a,b"," {{a:1 2 231 2, b: 3 3 1 3, c: 4 8 232 8, d: 4 8 232 8}} ");
  TEST_EQUAL("a: 1 1 1 2 2 2; update {{a:a, b: 3 3 4 3 4 4}} set b = sum(a) group by a,b "," {{a:a, b: 2 2 1 2 4 4}} ");
  TEST_EQUAL("UPDATE {{a:0 1 2, b:['Eh','Bee','Sea']}} SET b='Dee'","{{a:0 1 2, b:repeat(3,'Dee')}}");
  TEST_EQUAL("t: {{a:0 0 0 1 1 1, b: 1 2 3 4 5 6}}; update t set c=sum(a) group by a; t","{{a:0 0 0 1 1 1, b: 1 2 3 4 5 6, c:0 0 0 3 3 3}}");
  TEST_EQUAL("t: {{a:0 0 0 1 1 1, b: 1 2 3 4 5 6}}; update t set c=sum(b) group by a; t","{{a:0 0 0 1 1 1, b: 1 2 3 4 5 6, c:6 6 6 15 15 15}}");
  TEST_EQUAL("t: {{a:0 0 0 1 1 1.0, b: 1 2 3 4 5 6}}; update t set c=sum(b) group by a; t","{{a:0 0 0 1 1 1.0, b: 1 2 3 4 5 6, c:6 6 6 15 15 15}}");
  TEST_EQUAL("foo:{[m]{}}; foo(1)","{}");
  TEST_EQUAL("10 mod 3","1");
  TEST_EQUAL("-10 mod 3","2");
  TEST_EQUAL("0 join mapleft 2 2 2","[0 2 2 2]");
  TEST_EQUAL("0 join mapleft 2","[0 2]");
  TEST_EQUAL("0 1 join mapleft 2 3","[0 2 3, 1 2 3]");
  TEST_EQUAL("0 join mapdown 1","[0 1]");
  TEST_EQUAL("join fold (join mapright mapleft) fold [2 3, 4 5]","[2 4, 2 5, 3 4, 3 5]");
  TEST_EQUAL("minus(2,1)","1");
  TEST_EQUAL("f:{[a,b] a - b}; f(2,1)","1");
  TEST_EQUAL("f:-; f(2,1)","1");
  TEST_EQUAL("car 1 2 4","1");
  TEST_EQUAL("car []","null");
  TEST_EQUAL("car 0 ^ 1 2","NAN");
  TEST_EQUAL("median 10 20 30","20.0");
  TEST_EQUAL("median 10 20 30 233","25.0");
  TEST_EQUAL("median 10 20 30 233 234","30.0");
  TEST_EQUAL("split(0 3 7, 'some text to split')",  " ['som', 'e te', 'xt to split']");
  TEST_EQUAL("split(3 7,   'some text to split')",  " ['som', 'e te', 'xt to split']");
  TEST_EQUAL("split(3, 'ABCDEF')","['ABC', 'DEF']");
  TEST_EQUAL("ngram(2, 'abcdefg') ","['ab','cd','ef','g']");
  TEST_EQUAL("g:[0 0, 0 0]; f: {[] for(i:0;i < 2;i: i+1){ for(j:0;j<2; j: j+1){.user.g[i][j]:1}}}; f(); g"," [1 1, 1 1]");
  TEST_EQUAL("g:[0 0, 0 0]; f: {[] global g; for(i:0;i < 2;i: i+1){ for(j:0;j<2; j: j+1){ g[i][j]:1}}}; f(); g"," [1 1, 1 1]");
  TEST_EQUAL("g:[0 0, 0 0]; f: {[] for(i:0;i < 2;i: i+1){ for(j:0;j<2; j: j+1){ g[i][j]:1}}}; f(); g"," [1 1, 1 1]");
  TEST_EQUAL("g:[0 0, 0 0]; f: {[] for(i:0;i < 2; i +: 1){ for(j:0;j<2; j +: 1){ .user.g[i][j]:1}}}; f(); g"," [1 1, 1 1]");
  TEST_EQUAL("g:[0 0 0, 0 0 0, 0 0 0]; f: {[] for(i:0;i < 3;i: i+1){ for(j:0;j<3; j: j+1){g[i][j]:1}}}; f(); g"," [1 1 1, 1 1 1, 1 1 1]");
  TEST_EQUAL("g:[0 0 0, 0 0 0, 0 0 0]; f: {[] for(i:0;i < 3;i: i+1){ for(j:0;j<3; j: j+1){g[i][j]: (1 - (g[i][j])) }}}; f(); g"," [1 1 1, 1 1 1, 1 1 1]");
  TEST_EQUAL("g:0 9 18 +mapleft ngram(3,range 9); [g[0][1][2],g[0][2][1],g[1][0][2],g[1][2][0],g[2][0][1],g[2][1][0]]","[5, 7, 11, 15, 19, 21]");
  TEST_EQUAL("g:0 9 18 +mapleft ngram(3,range 9);g[0][1][2]:235,g[0][2][1]:2,g[1][0][2]:3,g[1][2][0]:4,g[2][0][1]:5,g[2][1][0]:6;[g[0][1][2],g[0][2][1],g[1][0][2],g[1][2][0],g[2][0][1],g[2][1][0]]","[235,2,3,4,5,6]");
  TEST_EQUAL("count unique deal(236,236)","236");
  TEST_EQUAL("count unique deal(4,{a:1,b:2,c:3,d:4})","4"); //seed_prng for specifics
  TEST_EQUAL("seed_prng(0); shuffle('BLAH')","'AHLB'");
  TEST_EQUAL("powerset 0 1", "[INT[],[1],[0],[0,1]]");
  TEST_EQUAL("permutations(INT[])","[INT[]]");
  TEST_EQUAL("permutations([])","[[]]");
  TEST_EQUAL("permutations([1])","[[1]]");
  TEST_EQUAL("permutations([1,2])","[[1,2],[2,1]]");
  TEST_EQUAL("permutations([1,1], false)","[[1,1]]");
  TEST_EQUAL("permutations([1,1], true)","[[1,1],[1,1]]");
  TEST_EQUAL("combinations([],1)","[]");
  TEST_EQUAL("combinations(INT[],1)","[]");
  TEST_EQUAL("combinations(3 2 2, 2)","[3 2]");
  TEST_EQUAL("combinations(3 2 2, 2, false)","[3 2]");
  TEST_EQUAL("combinations(3 2 2, 2, true)","[3 2, 3 2, 2 2]");
  TEST_EQUAL("combinations(3 2 1, 2, false)","[[3, 2], [3, 1], [2, 1]]");
  TEST_EQUAL("combinations(3 2 1, 3, false)","[3 2 1]");
  TEST_EQUAL("combinations(3 2 1, 0, false)","[INT[]]");
  TEST_EQUAL("combinations(3 2 1, 4, false)","[]");
  TEST_EQUAL("combinations(3 2 1,-1, false)","[]");
  TEST_EQUAL("({[y] y} mapcores) 200 2 3 201","200 2 3 201"); 
  TEST_EQUAL("{[x] x mod 2} filter 4 5 6 7 8 9","[5, 7, 9]");
  TEST_EQUAL("{[x] x mod 2} filter {a:27,b:44,c:97,d:20}","{a:27, c:97}");
  TEST_EQUAL("(not isnull) filter [3,null,27,nil,39]","[3, 27, 39]");
  TEST_EQUAL("(not isnull) filter {foo:null,bar:'ABC',quux:nil}","{bar:\"ABC\"}");
  TEST_EQUAL("t:{}; t['a']['b']:1;t", "{a:{b:1}}");
//These cases won't work without a lot of additional ragged-checking logic
//  TEST_EQUAL("t:{{a:1 2 3, b:4 5 6}}; t[null]: 237; t['a']", "[237]");
//  TEST_EQUAL("t:{{a:1 2 3, b:4 5 6}}; t[null]: [238]; t['b']", "[238]");
  TEST_EQUAL("t:{{a:1 2 3, b:4 5 6}}; t[null, 0]: 239","{{a:239 2 3,b:239 5 6}}");
  TEST_EQUAL("t:{}; t[['a','b','c'],['d','e','f']]:240","{a:{d:240, e:240, f:240}, b:{d:240, e:240, f:240}, c:{d:240, e:240, f:240}}");
  TEST_EQUAL("t:{}; t['e','d']:241","{e:{d:241}}");
  TEST_EQUAL("t:{a:1 2 3}; t[]: 4 5 6 ","{a:[4, 5, 6]}");
  TEST_EQUAL("t:{a:1 2 3, b:7 8 9}; t[]: 4 5 6","{a:[4, 5, 6], b:[4, 5, 6]}");
  TEST_EQUAL("t:{{a:1 2 3}}; t[]: 4 5 6","{{a:4 5 6}}");
  TEST_EQUAL("t:{{a:1 2 3, b:7 8 9}}; t[]: 4 5 6","{{a:4 5 6, b:4 5 6}}");
  TEST_EQUAL("float ['foo', 0.4, 3]","[0,0.4,3.0]");
  TEST_EQUAL("kerf_type 1 drop ['a',2.0,3.0]","-3");
  TEST_EQUAL("int([5, 3.2, `A])","[5, 3, 65]");
  TEST_EQUAL("int('text')","[116, 101, 120, 116]");
  TEST_EQUAL("parse_int '4500'","4500");
  TEST_EQUAL("parse_int('100', 16)","256");
  TEST_EQUAL("parse_int(['Ab', 'cD', 'eF'], 16)","[171, 205, 239]");
  TEST_EQUAL("parse_float '23.4'","23.4");
  TEST_EQUAL("parse_float '1'","1.0");
  TEST_EQUAL("parse_float '10e3'","10000.0");
  TEST_EQUAL("parse_float ['12.1', '-7.8']","[12.1, -7.8]");
  TEST_EQUAL("2015.01.02 + 1y1m + 10d","2016.02.12");
  TEST_EQUAL("2015.01.01 + 1y + 1m1d + 1s","2016.02.02T00:00:01");
  TEST_EQUAL("2015.01.02 - 1y","2014.01.02");
  TEST_EQUAL("rep 1y1m1d","'1y1m1d'");
  TEST_EQUAL("json_from_kerf 2015.01.01","'\"KERFSTAMP_2015.01.01\"'");
  TEST_EQUAL("table(['foo','bar'],[1 2,3 4])","{{foo: 1 2, bar: 3 4}}");
  TEST_EQUAL("table([],[])","{{}}");
  TEST_EQUAL("2016-01-24T01:02:03","2016.01.24T01:02:03");
#if DATES_ALLOW_DASHED
  TEST_EQUAL("2016-01-24","2016.01.24");
#else
  TEST_EQUAL("2016-01-24","2039");
#endif
  TEST_EQUAL("unzip zip range 1000","range 1000");
  TEST_EQUAL("kerf_type_name stamp[]","'stamp vector'");
  TEST_EQUAL("sqrt unfold 0.0", "[0.0]");
  TEST_EQUAL("sqrt unfold float range 1", "[[0.0]]");
  TEST_EQUAL("sqrt fold float range 5", "0 1 1 1 1.0");
  TEST_EQUAL("a:{{aa: 1 2 251, bb:enum['o', 'p', 'f']}};  update a set bb=['z','r','d']; a['bb'][0]","'z'");
  TEST_EQUAL("a:{{aa: 1 2 252, bb:index['s', 'q', 'a']}}; update a set bb=['z','r','d']; a['bb'][0]","'z'");
  TEST_EQUAL("a:#['b','c']; a[-1 -1]","#[null, null]");
  TEST_EQUAL("a:#1 2 3; a[-1 -1]","#NAN NAN");
  TEST_EQUAL("delete_keys({a:1,b:2,c:3},['c','a'])","{b:2}");
  TEST_EQUAL("delete_keys({a:4,b:5,c:6},['c','foo','c'])","{a:4,b:5}");
  TEST_EQUAL("delete_keys({{a:5,b:9}},['a'])","{{b:9}}");
  TEST_EQUAL("delete_keys({{a:6,b:8}},['f','a'])","{{b:8}}");
  TEST_EQUAL("mmin(range(100),10)","10"); //adverb bug94
  TEST_EQUAL("format('%s %d %f %%',['ab',2,9])","'ab 2 9.000000 %'");
  TEST_EQUAL("format('%4s|%.3d|%.2f',['ab',2,9.1234])","'  ab|002|9.12'");
  TEST_EQUAL("format_stamp('%H|%M', 12:00:15)",  "'12|00'");
  TEST_EQUAL("format_stamp('%H|%M', [12:00:15, 12:01:15])", "['12|00', '12|01']");
  TEST_EQUAL("format_stamp('%H:%M:%S.%Q',1999.01.01T09:30:01.90)","'09:30:01.900000'");
  TEST_EQUAL("format_stamp('%H:%M:%S.%q',1999.01.01T09:30:01.90)","'09:30:01.900'");
  TEST_EQUAL("format_stamp('%H:%M:%S.%N',1999.01.01T09:30:01.90)","'09:30:01.900000000'");
  TEST_EQUAL("COMPRESSED(10 20 30 40)[1 3]","20 40");
  TEST_EQUAL("extract select from {{}}","[]");
  TEST_EQUAL("extract select a from {{a:1 2 258}}","1 2 258");
  TEST_EQUAL("extract select a from {{a:1 2 259}} where a <= 1","1");
  TEST_EQUAL("extract select a,b from {{a:1 2 3, b:4 5 6, c: 7 8 260}}","[1 2 3, 4 5 6]");
  TEST_EQUAL("extract select a from {{a:1 2 3, b:4 5 6, c: 7 8 261}}","1 2 3");
  TEST_EQUAL("extract select b from {{a:1 2 3, b:4 5 6, c: 7 8 261}} where a = 1","4");
  TEST_EQUAL("extract select a,b from {{a,b}}",            "[[],[]]");
  TEST_EQUAL("extract select a,b from {{a:1,b:262}}",      "1 262");
  TEST_EQUAL("extract select a,b from {{a:1 2,b:263 264}}","[1 2,263 264]");
  TEST_EQUAL("parse_stamp('%H:%M:%S','20:55:33')", "20:55:33");
  TEST_EQUAL("parse_stamp('%H:%M.%Q:%S', '12:34.789:56')", "12:34:56.789");
  TEST_EQUAL("parse_stamp('%H', ['3','4','5'])","[03:00:00.000, 04:00:00.000, 05:00:00.000]");
  TEST_EQUAL("bars(5, 0 3 6 9 12)"," 0 0 5 5 10");
  TEST_EQUAL("seed_prng(100); deal(1,{{a:1 2 3,b:4 5 6}})","{{a:3,b:6}}");
  TEST_EQUAL("bars(5d, 2016.06.01 + 1d + mapright range(9) )","repeat(4,2016.06.01) join repeat(5,2016.06.06)");
  TEST_EQUAL("[2015.01.01T01:02:03, 2015.01.01T01:02:04, 2016.06.21, 2017.07.07]['year']","2015 2015 2016 2017");
  TEST_EQUAL("(,,)","NULL");
  TEST_EQUAL("(;;)","NULL");
  TEST_EQUAL("(())","NULL");
  TEST_EQUAL("[1, 2; 3]","[1,2,3]");
  TEST_EQUAL("plus fold [1, 2, 3]","6");
  TEST_EQUAL("1+(2+3;4+5)+6","16");
  TEST_EQUAL("len [1, 2, [3, 4], 5]","4");
  TEST_EQUAL("(3/10) + 1.5","1.8");
  TEST_EQUAL("null","NULL");
  TEST_EQUAL("`x","`'x'");
  TEST_EQUAL("`'fff'","`'f'");
  TEST_EQUAL("return 1; 2","1");
  TEST_EQUAL("plus(1,2)","3");
  TEST_EQUAL("1 2 3 (0)","1");
  TEST_EQUAL("(55 66 77 88 99) 1 2 3 (0)","66");
  TEST_EQUAL("negate 4","-4");
  TEST_EQUAL("negate fold 4","4");
  TEST_EQUAL("negate fold range 4","range(4)");
  TEST_EQUAL("join fold fold [[[0,1],[5,6]],[2,3]]","[0, 1, 5, 6, 2, 3]");
  TEST_EQUAL("join fold fold [[[0,1],[5,[6 7 8, [10 11]]]],[2,3]]","[0, 1, 5, 6, 7, 8, 10, 11, 2, 3]");
  TEST_EQUAL("join fold converge [[[0,1],[5,6]],[2,3]]","[0, 1, 5, 6, 2, 3]");
  TEST_EQUAL("[1+1,2*3,4/8] + 1","[3, 7, 1.5]");
  TEST_EQUAL("!0","1");
  TEST_EQUAL("!1","0");
  TEST_EQUAL("!1 0 1 0 1","[0, 1, 0, 1, 0]");
  TEST_EQUAL("-[0,1,2,[1]]","[0,-1,-2,[-1]]");
  TEST_EQUAL("![0,1,2,[1]]","[1,0,0,[0]]");
  TEST_EQUAL("![0,1,2,`c]","[1,0,0,0]");
  TEST_EQUAL("![0,1,2]","[1,0,0]");
  TEST_EQUAL("not [1, 'c']","[0,0]");
  TEST_EQUAL("not [0,1,2,'c']","[1,0,0,0]");
  TEST_EQUAL("[0 1, 2 3][0][1]","1");
  TEST_EQUAL("0+0","0");
  TEST_EQUAL("[[0,1],[2,3]][0][1]","1");
  TEST_EQUAL("[[0,1],[2,3]][1][0]","2");
  TEST_EQUAL("[[0,1],[2,3]][1]","[2,3]");
  TEST_EQUAL("1 2 3[1]","2");
  TEST_EQUAL("1 2 3[1 0]","[2,1]");
  TEST_EQUAL("1 2 3[0 1 2;0 0]","[[1,1],[2,2],[3,3]]");
  TEST_EQUAL("1 + reverse([1, 2, 3])","[4,3,2]");
  TEST_EQUAL("3<=2","0");
  TEST_EQUAL("{[b]1 + b} 244","245");
  TEST_EQUAL("3*10","30");
  TEST_EQUAL("2**10","1024.0");
  TEST_EQUAL("{[b]10 + b} 244","254");
  TEST_EQUAL("{[b]100 + b} 244","344");
  TEST_EQUAL("(1+,2+) 1","3");
  TEST_EQUAL("(0+, 1+ ) 2","3");
  TEST_EQUAL("a:","NULL");
  TEST_EQUAL("a:;a","NULL");
  TEST_EQUAL("a: 2 3 4","[2,3,4]");
  TEST_EQUAL("a: 2 3 4;a[0] : 5","[5,3,4]");
  TEST_EQUAL("a: 2 3 4;a[0] +: 5","[7,3,4]");
  TEST_EQUAL("a: 2 3 4;a[0] plus: 5","[7,3,4]");
  TEST_EQUAL("a: 2 3 4;a reverse:","[4,3,2]");
  TEST_EQUAL("a: 2 3 4;a[0] reverse:","[2,3,4]");
  TEST_EQUAL("a: 2 3 4; a -:","[-2,-3,-4]");
  TEST_EQUAL("a: 2 3 4; a -: 1","[1,2,3]");
  TEST_EQUAL("a: 3 4;(a+) (a+) (a+) 1","[10,13]");
  TEST_EQUAL("{[x]1+x} {[x]1+x} {[x]1+x} 0","3");
  TEST_EQUAL("a:3 4;  {[q] a+1} 0","[4,5]");
  TEST_EQUAL("{[]1+1} 0","2");
  TEST_EQUAL("{[] - 1} 0","-1");
  TEST_EQUAL("{[]1} 0","1");
  TEST_EQUAL("{[]} 0","{}");
  TEST_EQUAL("a: 3 4; {[q]a:4; a+1} 0","5");
  TEST_EQUAL("a:4; a+1","5");
  TEST_EQUAL("a: 4; {[a] 1+a; b:2; b} 0","2");
  TEST_EQUAL("A:{a:1, b:2}; A+A","{a:2, b:4}");
  TEST_EQUAL("{'a':1, b:2}","{a:1, b:2}");
  TEST_EQUAL("f:4 5 6; f mapdown 0 1 0","[4,5,4]");
  TEST_EQUAL("+ fold 1 2 3","6");
  TEST_EQUAL("f:{[x,y]x+y}; f(1,2)","3");
  TEST_EQUAL("+ fold range 1000000","499999500000");
  TEST_EQUAL("f:{[x,y]x+y}; f fold range 1000","499500");
  TEST_EQUAL("a:range(1000);plus fold a","499500");
  TEST_EQUAL("1 + refold 1 2","[1,2,4]");
  TEST_EQUAL("1 + fold 1 2","4");
  TEST_EQUAL("plus fold 1 2 3","6");
  TEST_EQUAL("plus fold 1 2 3.0","6.0");
  TEST_EQUAL("plus fold 1.0","1.0");
  TEST_EQUAL("1 minus fold 1 2 3","-5");
  TEST_EQUAL("1 plus fold 1 2 3.0","7.0");
  TEST_EQUAL("1 plus fold 1","2");
  TEST_EQUAL("1 plus fold 1.0","2.0");
  TEST_EQUAL("-1 minus fold -1 -2 -3","5");
  TEST_EQUAL("1 2 plus fold 1.0 2.0","[4,5.0]");
  TEST_EQUAL(">= fold 1 2 3","0");
  TEST_EQUAL("a:1; do(10){}; a","1");
  TEST_EQUAL("a:1; do(10){11}","11");
  TEST_EQUAL("(+) fold 1 2 3","6");
  TEST_EQUAL("a:1; if(1){a:2}; a","2");
  TEST_EQUAL("a:1; if(0){a:2}else if(1){a:3}; a","3");
  TEST_EQUAL("a:1; if(0){a:2}else if(1){a:3} else if (20){a:4}; a","3");
  TEST_EQUAL("a:1; if(1){a:2} else {a:5}; a","2");
  TEST_EQUAL("a:1; if(0){a:2} else if(0){a:4} else {a:5}; a","5");
  TEST_EQUAL("a:1; if(0){a:2} else if(0){a:3} else if(0){a:4} else {a:5}; a","5");
  TEST_EQUAL("b:1; for(a:1; a<0; a+:1){ b+:1; }; b","1");
  TEST_EQUAL("b:1; for(a:1; a<3; a+:1){ b+:1; }; b","3");
  TEST_EQUAL("b:1; for(a:1; a<10; a+:1){ b+:1; }; b","10");
  TEST_EQUAL("a:5; do(2){a +: 1}; a","7");
  TEST_EQUAL("- fold 1 2 3","-4");
  TEST_EQUAL("a:5; while(a>0){a -: 2}; a","-1");
  TEST_EQUAL("2015.05.05 = 2015y","1");
  TEST_EQUAL("2015.05.05 > 2014y","1");
  TEST_EQUAL("2014.05.05 < 2015y","1");
  TEST_EQUAL("2015y = 2015.05.05","1");
  TEST_EQUAL("2015.05.05 = 2015y05d","1");
  TEST_EQUAL("2015.05.05 = 2015y05m","1");
  TEST_EQUAL("2015.05.05 = 2015y05m05d","1");
  TEST_EQUAL("2015.05.05 == 2015y05m05d","1");
  TEST_EQUAL("2015y05m05d == 2015.05.05 ","1");
  TEST_EQUAL("2014.05.05 2014.05.06 < 2015y05m","[0,0]");
  TEST_EQUAL("2014y > 2015.05.05","0");
  TEST_EQUAL("2015y < 2014.05.05","0");
  TEST_EQUAL("2015.05.05 <> 2015y05m05d","0");
  TEST_EQUAL("2015.05.05 != 2015y05m05d","0");
  TEST_EQUAL("2015.06.04 >  2015y05m05d","0");
  TEST_EQUAL("2015.04.06 <  2015y05m05d","0");
  TEST_EQUAL("2015.04.06 <= 2015y05m05d","0");
  TEST_EQUAL("2015.06.04 >= 2015y05m05d","0");
  TEST_EQUAL("2015.05.05 > 2014y05m","0");
  TEST_EQUAL("2014.05.05 < 2015y05m","0");
  TEST_EQUAL("2015.05.05 > 2014y05m","0");
  TEST_EQUAL(" 2015y05m05d >  2015.06.04 ","0");
  TEST_EQUAL(" 2015y05m05d <  2015.04.06 ","0");
  TEST_EQUAL(" 2015y05m05d <= 2015.04.06 ","0");
  TEST_EQUAL(" 2015y05m05d >= 2015.06.04 ","0");
  TEST_EQUAL("t:{{a}}; do(3){insert into t values (123)};t","{{a: 123 123 123}}");
  TEST_EQUAL("t:{{a}}; insert into t values [range(10)]; t","{{a:range(10)}}");
  TEST_EQUAL("a:11 22 33; a[1 0]:a[0 1]","[22,11,33]");
  TEST_EQUAL("range(0,20,5)","[0,5,10,15]");
  TEST_EQUAL("range(4,25,5)","[4,9,14,19,24]");
  TEST_EQUAL("range(4,5,5)","[4]");
  TEST_EQUAL("range(4,3,5)","INT[]");
  TEST_EQUAL("which 0 0 0 -1 0 1 2 3","[5, 6, 6, 7, 7, 7]");
  TEST_EQUAL("m:{a:1}; m['a'] #: 1","{a:[1, 1]}");
  TEST_EQUAL("m:{a:1}; m['b']; m","{a:1}");
  TEST_EQUAL("m:{a:1}; m['b']","NULL");
  TEST_EQUAL("m:{a:1}; m['b'] #: 1","{a:1, b:[null, 1]}");
  TEST_EQUAL("distinct 1 1 2 2 1 3 3 3 4 2 2","[1,2,3,4]");
  TEST_EQUAL("transpose [1 2, 4 5, 'abc', 6 ]","[[1,4,'abc',6], [2,5,'abc', 6]]");
  TEST_EQUAL("1 2 join mapleft  mapright 4 5","[[[1,4],[2,4]],[[1,5],[2,5]]]");
  TEST_EQUAL("1 2 3 +\\> 4 5 6","[[5, 6, 7],[6, 7, 8],[7, 8, 9]]");
  TEST_EQUAL("{[x,y] x join mapright mapleft y } fold [1 2, 4 5]","[[[1,4], [1,5]], [[2, 4],[2, 5]]]");
  TEST_EQUAL("{[x,y] x join y } fold [1 2, 4 5]","[1,2,4,5]");
  TEST_EQUAL("join fold [1 2, 4 5]","[1,2,4,5]");
  TEST_EQUAL("(1-)3","-2");
  TEST_EQUAL("2+f:1","3");
  TEST_EQUAL("f:1+; f 1","2");
  TEST_EQUAL("f:1+; g:2+; f g 1","4");
  TEST_EQUAL("sum [1, 2, 3, 4]","10");
  TEST_EQUAL("sum [1 2, 2, 3, 4]","[10,11]");
  TEST_EQUAL("avg 2 2 3 3","2.5");
  TEST_EQUAL("max index 2 2 3 3 2 2","3");
  TEST_EQUAL("var 1 2 3","2/3");
  TEST_EQUAL("{[x,y] x take enlist y}(3,'ca')","repeat(3,'ca')");
  TEST_EQUAL("(1+)3","4");
  TEST_EQUAL("((1+)) 3","4");
  TEST_EQUAL("join fold repeat(2**1, 'fun')","'funfun'");
  TEST_EQUAL("f:{[x,y] x - y}; f(3,2)","1");
  TEST_EQUAL("a: [[00,11]; [22,33]]; a[1,0]","22");
  TEST_EQUAL("a: [[00,11]; [22,33]]; a[1][0]","22");
  TEST_EQUAL("a: {[ {a:1,b:2}, {b:4,c:5}, {b:{d:6,e:7}} ]}; select from a where 3<b","atlas[{b:4, c:5}]");
  TEST_EQUAL("a: {[ {c:10,b:4,a:1}, {b:4,c:10,a:2}, {c:11, b:5,a:3} ]}; select sum(a) from a group by b,c where a>=2","{{b:4 5, c:10 11, a: 2 3}}");
  TEST_EQUAL("select from {{a: #['faaa','bbb','ccc']}} where a = 'bbb'","{{a:#['bbb']}}");
  TEST_EQUAL("select from {{a: #['faaa','bbb','ccc']}} where a = 's'","{{a:#[]}}");
  TEST_EQUAL("a:{{b:1 2 403, c: 4 5 6, d: 7 8 9}}; a[['b']]","{{b:1 2 403}}");
  TEST_EQUAL("a:{{b:1 2 404, c: 4 5 6, d: 7 8 9}}; a[['c','b']]","{{c:4 5 6, b:1 2 404}}");
  TEST_EQUAL("['a','c','d','b'] in {a:'f', b:'c'}","1 0 0 1");
  TEST_EQUAL("HASH(['a','b','c']) join HASH(['c','d','e'])","#['a','b','c','c','d','e']");
  TEST_EQUAL("flatten(repeat(1000,#['a','b','c']))='a'","flatten(repeat(1000,[1,0,0]))");
  TEST_EQUAL("flatten(repeat(1000,#['a','b','c']))='b'","flatten(repeat(1000,[0,1,0]))");
  TEST_EQUAL("flatten(repeat(1000,#['a','b','c']))='c'","flatten(repeat(1000,[0,0,1]))");

  TEST_EQUAL("extract select from {{a:range 3}} where -1 <  a","0 1 2");
  TEST_EQUAL("extract select from {{a:range 3}} where -1 >  a","INT[]");
  TEST_EQUAL("extract select from {{a:range 3}} where -1 <= a","0 1 2");
  TEST_EQUAL("extract select from {{a:range 3}} where -1 >= a","INT[]");
  TEST_EQUAL("extract select from {{a:range 3}} where -1 <> a","0 1 2");

  TEST_EQUAL("extract select from {{a:range 3}} where  9 <  a","INT[]");
  TEST_EQUAL("extract select from {{a:range 3}} where  9 >  a","0 1 2");
  TEST_EQUAL("extract select from {{a:range 3}} where  9 <= a","INT[]");
  TEST_EQUAL("extract select from {{a:range 3}} where  9 >= a","0 1 2");
  TEST_EQUAL("extract select from {{a:range 3}} where  9 <> a","0 1 2");

  TEST_EQUAL("extract select from {{a:range 3}} where  a <  9","0 1 2");
  TEST_EQUAL("extract select from {{a:range 3}} where  1 <> a","0 2");

  TEST_EQUAL("extract select from {{a:3 3 3}} where  3 <> a","INT[]");
  TEST_EQUAL("extract select from {{a:3 3 3}} where  2 <> a","3 3 3");

  TEST_EQUAL("","");
  TEST_EQUAL("","");
  TEST_EQUAL("","");
  TEST_EQUAL("","");
  TEST_EQUAL("","");
  TEST_EQUAL("","");
  TEST_EQUAL("","");
  TEST_EQUAL("","");

  TEST_EQUAL("skip","t:{{a: 0 0 0 1 1 1 2, PRICE:10 20 30 100 200 300 -100}}; flatten last xvals select msum(4,PRICE) from t group by a","[10, 30, 60, 100, 300, 600, -100]"); //don't sort in part()

  test_suite_finalize();
  return;
}




