#include "kerf.h"

//eval bytecode interpreter frames opcodes stack vm
//one reason for using LOOK_() here not kN() is that (void*)NULL stack entries become nil

__kerfthread VM0 KVM0 = {0};
__kerfthread VM  KVM = &KVM0;

bool The_Emu_Debug_Flag = false;

#pragma mark - VM Init/Free

int vm_init()
{
  local_vm_init(&KVM);
  return 0;
}

int local_vm_init(VM *a)
{
  VM m = *a;

  *m = (VM0){0};
  m->fake_header = kt0(EMU);

  m->KERFDIR = charvec_from_cstring(KERF_TREE_STARTING_DIRECTORY);

  I starting_height = POW2(16);//arbitrary starting height, [0, inf)
  m->stack = new_k(LIST, starting_height);
  SET_ATTR(m->stack, ATTR_FILLED);
  m->stack->n = starting_height;

  I work_starting_height = 0;//arbitrary starting height, [0, inf)
  m->workspace = new_k(-LINK, work_starting_height);
  m->workspace->n = work_starting_height;

  return 0;
}

void vm_dealloc(VM m)
{
  //NOTE: you need to free m->KERFDIR and such
  //before freeing the workspace,
  //and the KVM workspace after *literally all other objects*
  //since rd() will look *at the workspace* under certain
  //testing conditions
  rd(m->KERFDIR);

  m->stack->n = 0;//never free raw data inside
  rd(m->stack);

  m->workspace->n = 0;
  rd(m->workspace);

  *m = (VM0){0};
}

void vm_reset(VM m)
{
  //if KTREE references are "embedded" in the bytecode (indirection)
  //you don't have to worry when reference decrementing the stack

  while(m->local_pointer > 0)
  {
    pop(m);
  }

  //During normal execution n is zero.
  //If an exception occured, n may be positive.
  //On a reset we reference decrement 
  //any leftover objects.
  while(m->workspace->n > 0)
  {
    work_pop_rd(true);
  }

  m->function = NULL;
  m->code = NULL;

  DO(sizeof(m->registers_K)/sizeof(m->registers_K[0]), if(m->registers_K[i])rd(m->registers_K[i]))
  DO(sizeof(m->registers_K0)/sizeof(m->registers_K0[0]), m->registers_K0[i] = K00)
  DO(sizeof(m->registers_I)/sizeof(m->registers_I[0]), m->registers_I[i] = 0) 

  m->local_bottom = 0;
  m->local_pointer = 0;
  m->instruction_pointer = 0;
  m->function_bottom = 0;
  m->execution_depth = 0;
  m->sql_peek = NULL;
  m->stack = m->stack;
  m->workspace = m->workspace;
}

#pragma mark - Bytecode Tools

I read_log_2_n_bytes_as_integer(VM m, I g)//KCIF: Kerf Compressed Integer Format
{
  I v = 0;
  UC *p = (void *)&v;

  DO(POW2(g), *p++ = m->code[m->instruction_pointer++])

  SW(g)
  {
    CS(0,v=(C)(v&0xff))//cast required for negative
    CS(1,v=ntohs(v))
    CS(2,v=ntohl(v))
    CS(3,v=ntohll(v))
  }

  return v;
}

K0 log_2_n_bytes_from_integer(I i) //s->h -> log bytecount, s->buf = bytes
{
  K0 s = K00;

  int8_t  c = (C)i;//cast down
  int16_t d = i;
  int32_t e = i; 
  int64_t f = i;

  if(c==f)
  {
    sh = 0;
    c = c;
    memcpy(&s.buf, &c, POW2(sh));
  }
  else if(d==f)
  {
    sh = 1;
    d = htons(d);
    memcpy(&s.buf, &d, POW2(sh));
  }
  else if(e==f)
  {
    sh = 2;
    e = htonl(e);
    memcpy(&s.buf, &e, POW2(sh));
  }
  else
  {
    sh = 3;
    f = htonll(f);
    memcpy(&s.buf, &f, POW2(sh));
  }

  return s;
}

I read_compressed_integer(VM m)
{
  I g = read_byte(m); 
  return read_log_2_n_bytes_as_integer(m,g);
}

I read_uncompressed_integer(VM m)
{
  I v = 0;
  UC *p = (void *)&v;

  DO(sizeof(I), *p++ = m->code[m->instruction_pointer++])

  v=ntohll(v);

  return v;
}

I read_byte(VM m)
{
  return read_log_2_n_bytes_as_integer(m,0);
}

#pragma mark - Stack Methods

void ensure_stack_height_absolute(VM m, I n)
{
  if (n < m->stack->n) return;//technically not necessary to play this game
  I revised_m = ceiling_log_2(list_size_k(LIST,n));
  m->stack = expand(m->stack, revised_m);
  m->stack->n = max_possible_count_K(m->stack);
}

void ensure_stack_height_relative(VM m, I n)
{
  ensure_stack_height_absolute(m, m->local_pointer + n);
}

void pop(VM m)
{
  m->local_pointer--;
  nestset(m->stack, m->local_pointer, &K00);
}

void push_ri(VM m, K x, I increment)
{

  if(ALPHA_DISK_REFCOUNT)
  {
    //We may be able to get rid of this one now...but test first
    //does it even matter?
    if(IS_DISK(x) && IS_TENANT(x))
    {
      increment = false;
    }
  }

  nestset_ri_rd(m->stack, m->local_pointer, x, increment, false);
  m->local_pointer++;
}

#pragma mark - Working space for in-progress objects - No leaks on errors
K* work_push(K x)//Don't increment. Safety feature: can free after errors.
{
  //POTENTIAL_OPTIMIZATION_POINT
  //work stack thread safety is only necessary while
  //threads don't have their own KVM (VMs/stacks)

  bool safe = The_Thread_Safe_Flag;

  if(safe) pthread_mutex_lock(&The_Work_Stack_Mutex);


  //Do not modify via work pointers something you've pushed
  //unless you verify an add/push won't happen that changes the work pointer
  //and probably not even then
  KVM->workspace = cow_add(KVM->workspace, kl(x));//OK for x==NULL

  K *p = (K*) kP(KVM->workspace)+(KVM->workspace->n - 1);

  assert((*p) == x);

  if(safe) pthread_mutex_unlock(&The_Work_Stack_Mutex);

  return p;
}

K work_pop_rd(bool decrement)
{

  bool safe = The_Thread_Safe_Flag;

  if(safe) pthread_mutex_lock(&The_Work_Stack_Mutex);


  K x = KVM->workspace;
  //POTENTIAL_OPTIMIZATION_POINT: we can shrink workspace if it's ~4x too big
  I n = --xn;
  K y = xP[n];//LINK
  xP[n] = NULL;

  if(safe) pthread_mutex_unlock(&The_Work_Stack_Mutex);

  //POTENTIAL_OPTIMIZATION_POINT: compiler should eliminate this branch
  if(decrement){rd(y); return NULL;}//return NULL for safety
  return y;//for work_pop()---probably should ignore anywhere decrement==true
}

K work_pop_expect(K expecting)
{
  K got = work_pop();

  if(expecting) assert(got == expecting);

  return got;
}

K work_pop()//Don't decrement
{
  return work_pop_rd(false);
}

K work_pop_n(I n)
{
  K k = NULL;
  DO(n, k = work_pop())
  return k;
}

K work_pop_n_rd(I n, bool decrement)
{
  K k = NULL;
  DO(n, k = work_pop_rd(decrement))
  return k;
}


#pragma mark - Frames

I frame_height(VM m)
{
  return m->local_pointer - m->local_bottom;
}

void frame_open(VM m)
{
  ensure_stack_height_relative(m,1);
  push_ri(m, ki(m->local_bottom), false);
  m->local_bottom = m->local_pointer;
}

void frame_close(VM m)
{
  while(m->local_pointer > m->local_bottom) pop(m);
  if(m->local_pointer == 0) return;
  K0 o; 
  I p = peeko(m,0,o)->i;
  pop(m);
  m->local_bottom = p;
}

void function_apply(VM m)
{
  K0 o;
  K x = peeko(m,0,o);

  if(FUNC!=xt || (FUNC_KIND_LAMBDA != x->nk && FUNC_KIND_DERIVED_VERB != x->nk && FUNC_KIND_DYNAMIC_LIB != x->nk))
  {
    //not seriously - shim to simplify dev
    I other_functions = 0;
    I nyi = 1;
    dd(x->nk)
    show(x);
    assert(other_functions == nyi);
  }

  I height = frame_height(m);
  I putative = height - 1;//some args may be EMPTY_ATOM filler for projections

  //POTENTIAL_OPTIMIZATION_POINT
  //HACK
  //Probably better to fix this later in both function_apply and function_open
  //during the regular argument stuff (& projecting, etc.)
  bool reverso = true;

  reverso = (FUNC_KIND_LAMBDA == x->nk);


  if(reverso)
  {
    DO(putative/2, I low  = m->local_bottom + i;
                   I high = m->local_bottom + height + -i + -2;
    
                   K0 temp = *k0i(m->stack, low);
                   *k0i(m->stack, low)  = *k0i(m->stack, high);
                   *k0i(m->stack, high) = temp;
    )
  }



  I total_args = kN(x,FUNC_TOTAL_ARGS)->i;
  I missing = kN(x,FUNC_UNFILLED_ARGS)->i;
  I total_arglocals = ECOUNT(kN(x,FUNC_ARGLOCALS));

  I supplied = 0;//only accept real args from putative
  DO(putative, K x = k0i(m->stack, m->local_bottom + i); if(!IS_EMPTY_ATOM(x))supplied++)

  //Adverb-derived-verbs have arg floor 1
  //Others have their respective 2 (dyadic) or 1 (monadic) floor
  //For total args they should have the "max" value
  //For adverbs this should default to 2 
  //For variadic verbs it should default to the max allowed
  //
  //Monadic-derived  (+monadic) accepts [1,inf), does not project
  //Dyadic-derived    (+) accepts [2,inf), projects on [1]
  //Variadic-derived  (.) accepts [2,inf), projects on [1]
  //Adverb-derived   (+fold) accepts [1,inf), does not project basically
  //Technically we're fudging inf as +127 because we don't want to fool
  //with an attribute at the moment. Nobody is going to use {[a1;...;a127]...}
  //
  //The floor is necessary to get a proper projection. Without it you'd get
  //only +[a;] instead of |1+[ ........ I guess maybe it isn't necessary? 
  //necessary only if you want to be able to avoid projections? (nice?)

  //When I came back to tune this to add derived verbs,
  //it was done admittedly a little slapdash. So if something looks wrong
  //I probably am the cause.
  I is_derived = (x->nk == FUNC_KIND_DERIVED_VERB);

  //Didn't look at this for projection (hence disabled).
  //It possibly looks a lot like regular LAMBDA.
  //Although maybe I messed up by not adding bytecode to push args
  //or having it with DERIVED verbs for push help
  //or some combination of those
  //maybe not though since args don't have names?
  I is_dynamic = (x->nk == FUNC_KIND_DYNAMIC_LIB);

  I existing = total_args - missing;
  I dvp_argc_min = x->nf - existing;//derived-verb-[maybe projected]

  I apply_variadic = is_derived && (putative == supplied) && (1 || supplied >= 2)
                     && (dvp_argc_min <= supplied);
                     //&& (supplied <= dvp_argc_max);

  I should_project = !apply_variadic && (supplied < missing);


  //////////////////////////////////////
  //Disabled for now
  if(is_derived) should_project = false;
  if(is_dynamic) should_project = false;
  //////////////////////////////////////
  //Derived verbs dont have entries in the arglocals for args. so projection wont work
  //exactly. unless you mess with it some more. this is eating up a lot of time for
  //nothing so I am turning it off for now. One way to do it is to wrap the 
  //same compiled derived verb in a compiled lambda-projection wrapper C-method.
  //Probably this is better than modifying the guts of the derived verbs. 
  //as we sort of expect the user to do for now except via the console.
  //Example user projection: {[a;b] derived_verb(a,b)} projected on [x,EMPTY]

  I bad_valence = (!apply_variadic && 0 != total_args && (putative > total_args || supplied > missing));

  bad_valence |= is_dynamic && putative != x->nf; //while we're not projecting

  if(bad_valence) //do we want to turn this off for derived verbs?
  {
    //too many arg attempts f[1;;;;;;;;;]
    //too many supplied args f[1;2;3;4;5;6;7;8;9;...]

    ERROR(ERROR_VALENCE);
  }
  else if(should_project)
  {

    K y = copy(x);//the projection

    nestneu(y, FUNC_ARGLOCALS, cow(kN(y,FUNC_ARGLOCALS)));
    K arglocals = kN(y,FUNC_ARGLOCALS);
    nestneu(arglocals, VALUES, cow(kN(arglocals, VALUES)));
    K vals = kN(arglocals,VALUES);

    I j = 0, k = 0;
    I applied = 0;
    I skipped = 0;
    I skipmax = missing - supplied;

    while(applied < supplied)
    {
      K z = k0i(m->stack, m->local_bottom + j);

      if(IS_EMPTY_ATOM(z))
      {
        if(skipped < skipmax)//skip in provided and in retained arguments
        {
          j++;
          k++;
          skipped++;
          continue;
        }
        else//skip only in the provided list
        {
          j++;
          continue;
        }
      }

      while(!IS_EMPTY_ATOM(k0i(vals, k))) k++;

      K0 o;
      z = LOOK_(m->stack, m->local_bottom + j, o);

      nestset_ri_rd(vals, k, z, true, true);
      applied++;

      j++;
      k++;
    }

    y0[FUNC_UNFILLED_ARGS].i -= supplied;

    while(m->local_pointer > m->local_bottom) pop(m);
    push_ri(m, y, false);
  }
  else //continue with function execution
  {
    function_open(m);
    function_go(m);
  }

  return;
}

void function_open(VM m)
{
  //|ARGS| then |ARGS|FUNC| then apply/eval/dot
  //which yields
  //|ARGS|LOCALS|FUNC|OLD_FUNC|OLD_FUNC_BTM|OLD_INSTR_PTR|OLD_WORKSPACE_HT|OLD_LOCAL_BTM|
  //^func bottom                                                         new local bottom^
  //if we instead create a PROJECTION, this function frame doesn't happen
  //furthermore, ARGS is actually, say
  //|ARG0|ARG2| to which |FUNC_PROJECTION| is applied (with 2/4 projected args)
  //and if it completes the arg count then, say
  //|ARG0|ARG1|ARG2|ARG3|LOCALS|FUNC_PROJECTION|... will be the frame
  //after we reorder the args on the stack

  K0 o;
  K x = peeko(m,0,o);
  I putative = frame_height(m) - 1;//some args may be EMPTY_ATOM filler for projections

  I total_args = kN(x,FUNC_TOTAL_ARGS)->i;
  K arglocals = kN(x,FUNC_ARGLOCALS);
  I total_arglocals = ECOUNT(arglocals);
  I total_locals = total_arglocals - total_args;
  I unfilled = kN(x, FUNC_UNFILLED_ARGS)->i;

  //Set the function up for execution:

  //1. Remove the function from the top of the stack
  //2. Rewrite supplied arguments in the correct order with projected arguments
  //3. Write LOCALS
  //4. Return the function to the top of the stack
  //5. Write old values for restoring previous frame later
  I function_bottom = m->local_bottom;
  I function_index = m->local_pointer - 1;
  I function_workspace_height = m->workspace->n;

  I more = total_arglocals - putative; 

  bool projecting = (unfilled < total_args);

  //Hack: projection turned off for derived verbs at the moment
  //      I think in order to turn it on you need to add another thing to the
  //      function data structure, like...wait, whoops
  //      TODO: I think in some places I'm getting FUNC_UNFILLED_ARGS and x->nf confused
  if(x->nk == FUNC_KIND_DERIVED_VERB)
  {
    more = MAX(0, more - (total_args - unfilled));
    projecting = false;
  }

  if(x->n == FUNC_KIND_DYNAMIC_LIB)
  {
    more = 0;
    projecting = false;
  }

  ensure_stack_height_relative(m, more);
  if(m->execution_depth >= VM_STACK_EXECUTION_MAX_DEPTH){ ERROR(ERROR_DEPTH); return;}
  DO(more, push_ri(m, kn, false))//move stack
  
  { //technically this should use an atomic swap
    *k0i(m->stack, function_index) = kn0;//wipe old function position
    nestset_ri_rd(m->stack, m->local_pointer - 1, x, false, false);//place new function position
    function_index = m->local_pointer - 1;//changed by pushes
  }
  //function is now on top like |ARGS|LOCALS|FUNC| except ARGS is unfinished (maybe) & LOCALS not set

  //Set ARGS/PROJ/LOCALS
  //A. Set Locals
  DO(total_locals, K0 o; K z = LOOK_(arglocals, total_args + i, o); nestset_ri_rd(m->stack, m->local_bottom + total_args + i, z, true, false))
  //B. Interleave Arguments/Projected Arguments
  if(projecting)//that is, if this is a projection being applied
  {
    I p = function_bottom, i = 0, j = 0;
    
    //Left compress supplied args (remove EMPTY ATOM projection things)
    I supplied = 0;
    DO(putative, K x = k0i(m->stack, function_bottom + i);
                 if(!IS_EMPTY_ATOM(x)){K0 t = *x; *x=K00; *k0i(m->stack, p++) = t; supplied++;}
    )

    //Walk backwards, interleave projected args
    K vals = kN(arglocals,VALUES);
    for (i = function_bottom + supplied - 1, j = total_args - 1; j >= 0; j--)
    {
      K x = k0i(vals, j);
      if(!IS_EMPTY_ATOM(x)) nestset_ri_rd(m->stack, function_bottom + j, kN(vals,j), true, false);
      else {x=k0i(m->stack, i--); K0 t = *x; *x=K00; *k0i(m->stack, function_bottom + j) = t;}
    }

  }
  
  {
    ensure_stack_height_relative(m, 4);
    push_ri(m, ki(m->function_workspace_height), false);
    push_ri(m, ki(m->function_index), false); 
    push_ri(m, ki(m->function_bottom), false); 
    push_ri(m, ki(m->instruction_pointer), false); 
  }

  frame_open(m);

  m->execution_depth += 1;

  //Set up function-frame
  //technically, this should be atomic too
  m->function_workspace_height = function_workspace_height;
  m->function_index = function_index; 
  m->function_bottom = function_bottom;
  m->function = x;
  m->code = kC(kN(x,FUNC_BYTECODE));
}

void function_push_derived_args_to_start(VM m)
{
  K y = m->function;
  //1. This calculation lets us calculate the number of supplied
  //   args for a variadic derived verb (not just "total_args")
  //   This insinuates it might make more sense to split args
  //   and locals; however, I think it's worse to do that
  //   for purposes elsewhere (determining arg vs. local in subfunction).
  //   But maybe not. You'd have to write it & profile it to tell.
  //
  //2. Another alternative is to rearrnge the args during setup
  //   instead of re-pushing (and hence reference incrementing) them.
  //   And recalculating here.

  I total_args = kN(y, FUNC_TOTAL_ARGS)->i;
  I total_arglocals = ECOUNT(kN(y,FUNC_ARGLOCALS));
  I total_locals = total_arglocals - total_args;
  I below = m->function_index - m->function_bottom;
  I argc = below - total_locals;

  DO(argc, push_arglocal(m,i))
}

void function_go(VM m)//may call any number of times after open
{

  K y = m->function;
  //Derived verbs need help getting their arguments onto the execution stack
  //once the function frame has been opened
  //Charvec/lambda/{} functions don't because they use PUSH ARG instructions
  SW(y->nk)
  {
    CS(FUNC_KIND_DYNAMIC_LIB,I n = y->nf; DO(n, push_arglocal(m,i)))
    CS(FUNC_KIND_DERIVED_VERB, function_push_derived_args_to_start(m))
  }

  m->instruction_pointer = 0;
}

void push_arglocal(VM m, I i)
{
  ensure_stack_height_relative(m, 1);
  K0 o;
  K x = LOOK_(m->stack, m->function_bottom + i, o);
  push_ri(m,x,true);
}

void function_close(VM m)
{
  K0 o[4];
  //prev bottom-->|ARGS|LOCALS|FUNC|...
  //...|OLD_FUNC|OLD_FUNC_BTM|OLD_INSTR_PTR|OLD_LOCAL_BTM|(<--current bottom)RESULTS

  //mimic close_bracket activity, but we could do whatever
  K x = kn;
  I u = frame_height(m);
  //take the result from the stack

  if(u > 0){x = peeko(m,0,o[0]); nestset_ri_rd(m->stack, m->local_pointer - 1, kn, false, false);}


  //We can't merely do a  frame_close  here (that was a bug)
  //since we might have other frames in progress
  //Close everything down inside the current *function* frame
  {
    //frame_close revised to close down to where the function started 
    //Note, if we put more stuff on top of function_index we need
    //to increase the 6 value
    while(m->local_pointer > m->function_index+6) pop(m);
    if(m->local_pointer == 0) return;
    I p = peeko(m,0,o[1])->i;
    pop(m);
    m->local_bottom = p;
  }

  //if this fails, we miscounted pushes+pops, and
  //so have a memory leak in the works
  if(IS_DEFINED(DEBUG) && m->workspace->n != m->function_workspace_height)
  {
    er(Workspace mismatch);
    dd(m->function_workspace_height);
    show(m->workspace);
    DO(m->workspace->n, show(kP(m->workspace)[i]))
  }
  assert(m->workspace->n == m->function_workspace_height);

  //POTENTIAL_OPTIMIZATION_POINT - we can look at most of this data directly 
  //POTENTIAL_OPTIMIZATION_POINT - we could try implicit recalc of function_bottom
  I instruction     = peeko(m,0,o[2])->i;
  I function_bottom = peeko(m,1,o[2])->i;
  I function_index  = peeko(m,2,o[2])->i;
  I function_workspace_height = peeko(m,3,o[2])->i;

  K func = LOOK_(m->stack, function_index, o[3]);

  if(FUNC == func->t)
  {
    m->function = func;
    m->code = kC(kN(func,FUNC_BYTECODE));
  }
  else
  {
    //we'll use this as a signal to stop looping
    //we could always use some other method
    m->function = NULL;
    m->code = NULL;
  }

  m->function_workspace_height = function_workspace_height;
  m->function_index      = function_index; 
  m->function_bottom     = function_bottom;
  m->instruction_pointer = instruction;
  m->execution_depth -= 1;

  while(m->local_pointer > m->local_bottom) pop(m);

  push_ri(m, x, false);//return the result to the stack
}

#pragma mark - Adverbs 

void adverb_open(VM m)
{
  m->instruction_pointer++;

  I supplied_args = frame_height(m);

  I preset_valence = read_compressed_integer(m);

  ADVERB_VARS0 vars = {0};

  vars.object[ADVERB_SUPPLIED_ARGS] = ki(supplied_args);
  vars.object[ADVERB_NOUN_OPERATOR] = kn;
  vars.object[ADVERB_PRESET_VALENCE] = ki(preset_valence);
  vars.object[ADVERB_WORKING_VALENCE] = ki(-1);
  vars.object[ADVERB_KIND] = ki(-1);
  vars.object[ADVERB_SUBKIND] = ki(SUBKIND_REGULAR);
  vars.object[ADVERB_i] = ki(0);//will be incremented by adverb while instructions
  vars.object[ADVERB_n] = ki(0);
  vars.object[ADVERB_FIRST] = kn;
  vars.object[ADVERB_PREVIOUS] = kn;
  vars.object[ADVERB_LATEST] = kn;
  //POTENTIAL_OPTIMIZATION_POINT
  //If this ends up being slow, you can change LISTs to support inline empty
  //LISTS of count 0, by being careful about what is increment/decremented
  //etc during expansion etc of lists. (Currently, this would cause problems) 
  //This change itself may cause slowness, potentially as much or more
  //in other parts of the app.
  //But this would only save time for repeated applications of the OVER cases
  //so possibly isn't worth it on those grounds
  vars.object[ADVERB_ACCUMULATOR] = new_k(LIST,0);

  ensure_stack_height_relative(m, ADVERB_SIZE);

  DO(ADVERB_SIZE, push_ri(m, vars.object[i], false))

  frame_open(m);
}        

//These macros let us easily work with adverb frame variables stored in the stack
#define ADVERB_POS(m, i) ((m)->local_bottom - (1 + ADVERB_SIZE) + (i)) 
#define ADVERB_GET(m, i) kN((m)->stack, ADVERB_POS(m, i))
#define ADVERB_SET(m, i, x, inc, dec) nestset_ri_rd((m)->stack, ADVERB_POS(m,i), (x), (inc), (dec))
//#define ADVERB_GET_ARG(m, j) ({LOOK(m->stack, m->local_bottom - (2 + ADVERB_SIZE) - (j));})
#define ADVERB_GET_ARG_(m, j, o) ({LOOK_(m->stack, m->local_bottom - (2 + ADVERB_SIZE) - (j),o);})

void adverb_init_working_valence(VM m)
{
  I preset_valence = ADVERB_GET(m, ADVERB_PRESET_VALENCE)->i;

  I work_valence = preset_valence;    

  if(SENTINEL==work_valence) //still => presetting was impossible; object on stack
  {
    //if frame_height != 1 ... 
    K0 o;
    K x = peeko(m,0,o);

    SW(xt)
    {
      CS(FUNC, SW(x->nk)
               {
                CS(FUNC_KIND_DERIVED_VERB, 
                    I height = frame_height(m);
                    I supplied = height - 1;

                    //er(supplies)
                    //dd(supplied)
                    //show(x);
                    //show_stack(m);

                    //if(supplied >= kN(x,FUNC_UNFILLED_ARGS)->i && supplied <= kN(x,FUNC_TOTAL_ARGS)->i)
                    //{
                    //  work_valence = supplied;
                    //  break;
                    //}

                    //Or constant 2? This line will probably interfere with projection of derived verbs
                    //(since really you would need to compute MAX ARGS minus PROJECTED ALREADY)
                    work_valence = kN(x,FUNC_TOTAL_ARGS)->i; break;
                 )
                 CD: work_valence = kN(x,FUNC_UNFILLED_ARGS)->i); break;
               }

      CD: work_valence = 1; //catch-all for noun juxtaposition and such
    }

    pop(m);//if valence uninitialized 
  }

  ADVERB_SET(m, ADVERB_WORKING_VALENCE, ki(work_valence), false, false);
}

I adverb_conformable(VM m, I start, I argc)
{
  I i; I n = 1;
  for(i = start; i < argc; i++)
  {
    K0 o;
    I c = ECOUNT(ADVERB_GET_ARG_(m,i,o)); 
    if(1==c)continue;
    SW(n)
    {
      CS(1, n = c)
      CD: if(c!=n)n=-1; break;
    }
  }

  //TODO: catching errors may need to reset things like WORKING_VALENCE
  //so that the init can reoccur...?
  //Is this even salvageable? Maybe the ___ needs to jump to
  //the nearest snippet or something? You could use the
  //function_text <-> instruction map, possibly
  //Possibly a stack_position <-> instruction_index is
  //necessary?
  //You could do a line-based version that (only) works with scripts
  if(-1==n)
  {
    ERROR(ERROR_CONFORMABLE);
  }            

  ADVERB_SET(m, ADVERB_n, ki(n), false, false);
  ADVERB_SET(m, ADVERB_SUBKIND, ki(SUBKIND_DO_N), false, false);

  return n;
}

void adverb_increment_i(VM m)
{
  k0i(m->stack, ADVERB_POS(m,ADVERB_i))->i++;
}

bool adverb_is_dupe(VM m)
{
  K x = ADVERB_GET(m, ADVERB_LATEST);

  K first = ADVERB_GET(m, ADVERB_FIRST);
  K prev  = ADVERB_GET(m, ADVERB_PREVIOUS);

  bool first_dupe = matchC(x, first, true);
  bool prev_dupe  = matchC(x, prev, true);

  bool dupe = first_dupe || prev_dupe;

  return dupe;
}

void adverb_init(VM m, I kind)
{
  //POTENTIAL_OPTIMIZATION POINT
  //In certain cases adverbs may be able to look ahead and pre-allocate more space

  //POTENTIAL_OPTIMIZATION POINT
  //gcc style labeled goto bytecode jumps? 20% bytecode speedup?

  adverb_init_working_valence(m);
  I op_valence = ADVERB_GET(m, ADVERB_WORKING_VALENCE)->i;//may have changed above
  ADVERB_SET(m, ADVERB_KIND, ki(kind), false, false);

  I argc = ADVERB_GET(m, ADVERB_SUPPLIED_ARGS)->i;

  assert(argc > 0);//How can we execute an adverb with no arguments?

  K0 o1,o2;
  K first_arg = ADVERB_GET_ARG_(m, 0,o1);
  K final_arg = ADVERB_GET_ARG_(m, argc - 1,o2);

  SW(kind)
  {
    CS(ADVERB_KIND_MAPDOWN,  adverb_conformable(m, 0,             argc))
    CS(ADVERB_KIND_MAPRIGHT, adverb_conformable(m, MIN(1,argc-1), argc))
    CS(ADVERB_KIND_MAPLEFT,  adverb_conformable(m, 0,             MAX(1,argc-1)))
   CSF(ADVERB_KIND_UNFOLD,)
    CS(ADVERB_KIND_FOLD,  
        SW(op_valence)
        {
          CSF(NILADIC,) CS(MONADIC, // f_monadic/[a_1;a_2;...;a_argc]
            ADVERB_SET(m, ADVERB_LATEST,   final_arg, true, true);
            ADVERB_SET(m, ADVERB_PREVIOUS, final_arg, true, true);
            SW(argc)
            {
              
              CD: ERROR(ERROR_VALENCE); break;//f_monadic/[a_1;a_2;a_3;...]
              CS(1, ADVERB_SET(m, ADVERB_FIRST, final_arg, true, true)) // f_monadic/a_1
              CS(2, // a_1 f_monadic/a_2
                  SW(first_arg->t)
                  {
                    CS(INT,
                        ADVERB_SET(m, ADVERB_n, ki(first_arg->i), false, false);
                        ADVERB_SET(m, ADVERB_SUBKIND, ki(SUBKIND_DO_N), false, false);
                      )
                    CS(FLOAT,
                        ADVERB_SET(m, ADVERB_n, ki(first_arg->f), false, false);
                        ADVERB_SET(m, ADVERB_SUBKIND, ki(SUBKIND_DO_N), false, false);
                      )
                    CD:
                      ADVERB_SET(m, ADVERB_SUBKIND, ki(SUBKIND_WHILE_B), false, false);
                      break;
                  }
                )
            }
          )
          CD: //f_dyadic/, f_triadic/, f_quartic/, ...
              ADVERB_SET(m, ADVERB_LATEST, first_arg, true, true);
              I n;
              SW(argc)
              {
                CS(1, n = adverb_conformable(m, 0, argc);
                      if(n>0)
                      {
                        K0 o;
                        ADVERB_SET(m, ADVERB_LATEST, LOOK_(first_arg,0, o), true, true);
                        ADVERB_SET(m, ADVERB_i, ki(1), true, true);//start ahead
                      }
                  )
                CD: n = adverb_conformable(m, 1, argc);
                    break;
              }
        }
      ) 
   CS(ADVERB_KIND_MAPBACK,
        SW(op_valence)
        {
         CD: ERROR(ERROR_VALENCE); break; //f_triadic': f_quartic': ... 
         CSF(NILADIC,) CS(MONADIC, // f_monadic':[a_1;a_2;...;a_argc]
              SW(argc)
              {
                CS(1, adverb_conformable(m, 0, argc))
                CD: ERROR(ERROR_VALENCE);
                    break;
              }
             )
          CS(DYADIC,  //f_dyadic': 
              ADVERB_SET(m, ADVERB_PREVIOUS, first_arg, true, true);
              I n;
              SW(argc)
              {
                CS(1, n = adverb_conformable(m, 0, argc);
                      if(n>0)
                      {
                        K0 o;
                        ADVERB_SET(m, ADVERB_PREVIOUS, LOOK_(first_arg,0, o), true, true);
                        //ADVERB_SET(m, ADVERB_i, ki(1), true, true);//start ahead
                        ADVERB_SET(m, ADVERB_n, ki(n - 1), true, true);//start ahead
                      }
                  )
                CD:   n = adverb_conformable(m, 1, argc);
                      break;
              }
            )
        }
     )
  }
}

void adverb_close(VM m)//tear down and place result
{
  m->instruction_pointer++;  

  frame_close(m);
  K0 o;
  K x = peeko(m,0,o);//grab accumulator
  nestset_ri_rd(m->stack, m->local_pointer - 1, kn, false, false);//wipe without reference decrementing

  //not simply DO(ADVERB_COUNT,...) b/c variable number of args
  while(m->local_pointer > m->local_bottom) pop(m);

  push_ri(m, x, false);//return the result to the stack
}

#pragma mark - Execution Methods

void recover_from_soft_jmp()
{
  bool soft_jmp_recovery_implemented = false;
  assert(soft_jmp_recovery_implemented);

  //probably:
  //1. close the current function context
  //1.5 maybe allow some stuff in the middle? user calls? from a new spawned kvm?
  //    can push KVM "onto a stack" via C function context, similar to jmp env stacking
  //2. restore the last function
  //3. pop+rd the work_stack to the restored workspace-function-height
  //4. have the restored function execute ... the previous instruction?
  //   does that even work? can we always count on it to be apply?
  //   whichever way, I don't think we're saving instruction_pointer_prev
  //   on the stack for the function frame, so we'd neeed to do that
  //5. let the calling C function handle restoring the jmp env pointers
  //6. you may or may not ultimately (or intermediately) want to leave
  //   a crafted error on top of the stack
  //   create_error_object_from_context(KVM, val);
  //
  //kevin 2016.04.29
  //If you intend to not-hard-jump from the net_kcall / json_net_call methods,
  //you prob. need to store the height of the work stack and the 
  //local stack pointer and pop back to where you were before the error.
  //Our strategy right now is to keep both of those (somewhat artificially)
  //at 0 going into the call so we can use hard jumps. (maybe this is good.)
  //The hard jump also cleans the KVM using vm_reset eg 'm->function = NULL;'
  //which is prob. necessary (to some extent) in the soft jump as well.
}

void fully_recover_from_hard_jmp()
{
  vm_reset(KVM);
  reset_lex_parse_globals();
}

int sigsetjmp_protected_ex(VM m, K function)
{
  volatile I r = -1;

  volatile int val;
  volatile sigjmp_buf *previous_global_env = soft_jmp_env;//"push"
  sigjmp_buf jmp_env;

  if(!(val = sigsetjmp(jmp_env, true)))
  {
    if(LONGJMP_NESTS)
    {
      soft_jmp_env = &jmp_env;
    }
  }
  else
  {
    //Caught Error 
    //printf("Error handling: %d\n",val);

    recover_from_soft_jmp();

    goto cleanup;

    //HINT: for error handling what we can do is
    //NOT branch to cleanup, but instead have stored
    //the fixed-size parts of the VM necessary to wind
    //the stack down to where it was when we initially
    //called sigsetjmp, and that will let us do a "redo"
    //Whenever we call sigsetjmp that sets the redo point.
    //(So it seems like we want, penalties aside, to call
    // sigsetjmp whenever we can as early as we can, 
    // whenever some portion of execution succeeds.)
    //If we need to display information before then closer
    //to where the error occurred, we display that before
    //using the rewind information to rewind the stack.
    //This rewind information may need to be `volatile`.
    //Don't forget to clean the work_push stack whatever you do.
    //
    //Note 1: Multiple Error Catching (2014.11.14)
    //We can probably do multiple siglongjmps, that is,
    //we can handle multiple nested stack errors, if
    //instead of trying to copy the sigjmp_buf, we use
    //a *pointer* to the sigjmp_buf's which are saved
    //volatile in each function call:
    //volatile sigjmp_buf *old_buf_ptr;
    //volatile sigjmp_buf local;
    //global_jmp_env_pointer = &local;
    //Then when you catch the error do:
    //global_jmp_env_pointer = old_buf_ptr;
    //I think that will work barring any weirdness in the
    //implementation of sigsetjmp.
    //Don't forget to reassign the global pointer to something
    //else when you're done executing, or somehow make the 
    //main() loop safe in some other way.
    //Reminder: you can't copy/assign sigjmp_buf's
    //Reminder: you can't call setjmp in a separate method
    //
    //Note 2: Recursion Depth (2014.11.14)
    //There's a subtle deficiency in our local_ex() method
    //where executing functions recursively, via that method,
    //consumes C-stack space instead of C-heap space. The
    //outcome is that our function recursion is susceptible
    //to stack overflows. What we want to do is to exit
    //the local_ex() call but have the parent continue
    //to execute the new function we've placed on the
    //KVM-stack, that is, the C-heap. In particular,
    //we want the while()execute_instruction(); loop to continue,
    //without the depth of the C-stack increasing. This probably
    //isn't that hard. One method that might work is to exploit
    //tail recursion for local_ex, but simple as that is it
    //might be more complicated than necessary. We might be 
    //able to do something trivial like split the while loop
    //out of local_ex and put it somewhere else.
    //
    //Note 3: Winding back to a specific instruction may be
    //a very bad idea. In particular, there are byetcode
    //instructions where problems can occur that need
    //some kind of setup overhead which won't be present on a
    //rewind. So possibly what you need to do is not go back to
    //a specific instruction, but rewind the entire function,
    //or somesuch (something larger and more general).
  }

  //CANJUMP
  //Note 1: We play a little loose here by not toggling this off later,
  //which would prevent CTRL+C jumps after sigsetjmp functions
  //have returned, say in main() after the attend loop.
  //(jumping to a returned function is a siglongjmp no-no) 
  //This is a long rabbit hole yielding zero returns
  //
  //Note 2: If we do something where we are rewinding to 
  //entire functions and not merely instructions, then we probably
  //need to disable jumping during the times when the function
  //data is being removed from the call stack/VM, namely,
  //during function_close(). The fear is that we will
  //use corrupted data.
  canjump = true;

  r = _local_ex(m, function);

cleanup:
  if(LONGJMP_NESTS)
  {
    soft_jmp_env = (sigjmp_buf *)previous_global_env;//"pop"
  }
  return r;
}

int local_ex(VM m, K function)
{
  return sigsetjmp_protected_ex(m, function);
}

K new_func_closed_from_stack(K function, VM m)
{
  K y = copy(function);

  nestneu(y, FUNC_ARGLOCALS, cow(kN(y,FUNC_ARGLOCALS)));
  K arglocals = kN(y,FUNC_ARGLOCALS);
  nestneu(arglocals, VALUES, cow(kN(arglocals, VALUES)));
  K vars = kN(arglocals,KEYS);
  K vals = kN(arglocals,VALUES);

  I args_only = kN(y, FUNC_TOTAL_ARGS)->i;
  I locals_only = COUNT(vars) - args_only;
  K parent_locals = kN(m->function, FUNC_ARGLOCALS);
  I parent_args = kN(m->function, FUNC_TOTAL_ARGS)->i;

  //SELF-LOCALS
  //PARENT-LOCALS
  //for each SELF-LOCAL, if PARENT-LOCAL has, pull off stack

  //Subfunctions contain all of the functions's locals up to the point
  //                                            ^should be arglocals ?
  //where the subfunction appears. So in {a:1;{x};b:2} the subfunctions
  //knows "a" but not "b". This would give a simple algorithm, except
  //subfunction arguments may obscure the parent function's locals.
  //Consider {a:1; {[a]a}; b:2}. In this case the subfunction
  //knows none of the local values of the parent function.
  //Hence we can either do an elaborate algorithm, we can create
  //some form of additional index, or we can default to hash lookups
  //for variables.

  //POTENTIAL_OPTIMIZATION_POINT
  //if this spot comes up in profiling, a few ways to proceed.
  //first is to do linear lookups for small small parent_locals
  //second is to use a more complicated algorithm for doing lookups

  DO(locals_only, 
    K0 o1,o2;
    K v = LOOK_(vars, args_only + i, o1);
    I p = lookupI(parent_locals, v); 
    if(IS_HASH_NULL(p)) break;
    I q = m->function_bottom + p;
    K z = k0i(m->stack, q);
    if(!IS_EMPTY_ATOM(z)) nestset(vals, args_only + i, LOOK_(m->stack, q, o2)); 
  )

  return y;
}

int _local_ex(VM m, K func)
{ 
  I u,v;
  K w,x,y,z;
  bool where_optimize = false;

  I local_ex_floor = m->local_bottom;

  ensure_stack_height_relative(m, 1);
  push_ri(m, func, true);
  function_apply(m);

  bool done = false;

  while (!done)
  {
    unsigned char inst = m->code[m->instruction_pointer];
    m->instruction_pointer_prev = m->instruction_pointer;

    //Trace execution
    //The_Emu_Debug_Flag = true;
    bool do_debug = The_Emu_Debug_Flag && The_Past_The_Test_Suite_Flag && The_Language_Initialized_Flag && IS_DEFINED(DEBUG);

    if(do_debug)
    {
      show_stack(m);
      fprintf(stderr, "Execute instruction: %x\n", inst);
      S lines = NULL; size_t chars = 0; I read = getline(&lines, &chars, stdin);
      show(kN(m->function, FUNC_BYTECODE));
      DO(2+ 2*m->instruction_pointer, O(" "))O("^^\n");
      //show(The_Kerf_Tree);
    }

    if (inst != RETURN_CODE) { The_Last_Expression_Was_Assignment_Flag = 0; }
    switch (inst)
    {
      default:
          fprintf(stderr, "Abort: invalid instruction: %x\n", inst);
          done = true;
          break;
 
      case NOOP:
          m->instruction_pointer++;
          break;

      case TABLE_RETURN:
      {
          K table = *work_push(new_table());
          K func = m->function;
          K arglocals = kN(func, FUNC_ARGLOCALS);
          K assigns   = kN(func, FUNC_LOCAL_ASSIGNS);
          //we depend on table columns being arrays only
          if(!IS_EMPTY_ATOM(assigns))
          ENUM(arglocals, K0 o; if(IS_HASH_NULL(lookupI(assigns,u)))continue;work_pop(); table = cow_table_add_column(table, u, LOOK_(m->stack, m->function_bottom + i, o)); work_push(table);)
          bool ragged = table_ragged(table);
          pop(m);
          push_ri(m, table, true);
          work_pop_rd(true);//now "*table" location doesn't exist
          if(ragged) ERROR(ERROR_RAGGED);
          wrapper_for_return(m, local_ex_floor, &done);
          break;
      }

      case ATLAS_RETURN:
      {
          //m->instruction_pointer++;
          K0 o;
          x = LOOK_(m->stack, m->local_pointer-1, o);
          K y = NULL;

          if(LIST==xt && 1==xn && kN(x,0)->t ==NIL) //special case {[ ]} off stack
          {
            y = new_atlas();
          }
          else
          {
            y = new_atlas_from_K(x);
          }

          pop(m);
          push_ri(m, y, false);

          wrapper_for_return(m, local_ex_floor, &done);
          break;
      }

      case MAP_RETURN:
      {
          K map = new_map();
          work_push(map);
          K func = m->function;
          K arglocals = kN(func, FUNC_ARGLOCALS);
          K assigns   = kN(func, FUNC_LOCAL_ASSIGNS);
          bool caught_table = false;
          K trigger = kcv(PARSE_JSON_TABLE_KEY);

          if(!IS_EMPTY_ATOM(assigns))
          ENUM(arglocals, if(IS_HASH_NULL(lookupI(assigns,u)))continue;
                          if(matchC(u,trigger,false))caught_table = true;
                          K0 o;
                          work_pop(); map = update(map, u, LOOK_(m->stack, m->function_bottom + i, o)); work_push(map);
          )

          pop(m);

          if(caught_table)
          {
            K table = new_table();

            ENUM(map, if(matchC(trigger, u, false))continue;
                      table = cow_table_add_column(table, u, v);
            )

            bool ragged = table_ragged(table);

            if(!ragged)
            {
              work_pop_rd(true);
              map = table;
              work_push(map);
            }
            else
            {
              rd(table);
            }
          }

          push_ri(m,map,true);
          work_pop_rd(true);
          wrapper_for_return(m, local_ex_floor, &done);
          break;
      }

      case RETURN_CODE:
          wrapper_for_return(m, local_ex_floor, &done);
          break;

      case STOP:
          //function_close(m);
          //O("STOP");
          done = true;
          //potentially we could do clean up here?
          break;

      case JMPA:
          m->instruction_pointer++;
          v = read_uncompressed_integer(m);
          m->instruction_pointer = v;
          break;

      case JMPRB:
          u = m->instruction_pointer;
          m->instruction_pointer++;
          v = read_uncompressed_integer(m);
          m->instruction_pointer = u + v;
          break;

      case JMPRF:
          m->instruction_pointer++;
          v = read_uncompressed_integer(m);
          u = m->instruction_pointer;
          m->instruction_pointer = u + v;
          break;

      case JMPRBIFN:
          u = m->instruction_pointer;
          m->instruction_pointer++;
          v = read_uncompressed_integer(m);

          x = NULL;
          if(0<frame_height(m)){K0 o; x = peeko(m,0,o); }
          if(x && falsy(x))
          { 
            m->instruction_pointer = u + v;
          }
          pop(m);
          break;

      case JMPRFIFN:
          m->instruction_pointer++;
          v = read_uncompressed_integer(m);
          u = m->instruction_pointer;

          x = NULL;
          if(0<frame_height(m)){K0 o; x = peeko(m,0,o); }
          if(x && falsy(x))
          { 
            m->instruction_pointer = u + v;
          }
          pop(m);
          break;

      case JMPRBIFNOTNIL:
          u = m->instruction_pointer;
          m->instruction_pointer++;
          v = read_uncompressed_integer(m);

          x = NULL;
          if(0<frame_height(m)){K0 o; x = peeko(m,0,o); }

          if(x && !IS_NIL(x))
          { 
            m->instruction_pointer = u + v;
          }
          else
          {

          }

          break;

      case JMPRFIFNOTNIL:
          m->instruction_pointer++;
          v = read_uncompressed_integer(m);
          u = m->instruction_pointer;

          x = NULL;
          if(0<frame_height(m)){K0 o; x = peeko(m,0,o); }
          if(x && !IS_NIL(x))
          { 
            m->instruction_pointer = u + v;
          }

          break;

      case POP:
          m->instruction_pointer++;  
          pop(m);
          break;

      case EMPTY_PUSH:
          ensure_stack_height_relative(m, 1);
          m->instruction_pointer++;  
          *k0i(m->stack, m->local_pointer++) = K00;
          break;

      case NIL_PUSH:
          ensure_stack_height_relative(m, 1);
          m->instruction_pointer++;  
          *k0i(m->stack, m->local_pointer++) = kn0;
          break;

      case ROOT_PUSH:
      {
          ensure_stack_height_relative(m, 1);
          m->instruction_pointer++;  
          //K root = The_Kerf_Tree;
          K0 o3;
          K1 k1;
          K root = denest_create(kcv(KERF_TREE_STARTING_DIRECTORY), true, o3, k1);
          push_ri(m, root, true);
          break;
      }

      case CONSTANT_PUSH: 
      {
          K0 o;
          m->instruction_pointer++;  
          v = read_compressed_integer(m);
          x = LOOK_(kN(m->function,FUNC_CONSTANTS),v,o);

          ensure_stack_height_relative(m, 1);

          SW(xt)
          {
            //grep keyword CONSTANT_PUSH_RECURSIVE_NOTE
            CD: push_ri(m, x, true); break;

            //Populate locals for closures/subfunctions
            CS(FUNC,  //assert: pushing empty atom xn==xt==(C)NULL is not possible. See EMPTY_PUSH
              K y = new_func_closed_from_stack(x, m);
              push_ri(m, y, false);
            )
          }

          break;
      }
      case GLOBAL_DYLIB_CALL:
      {
          ensure_stack_height_relative(m, 1);
          m->instruction_pointer++;  

          K0 o1,o2;
          K pair  = LOOK_(kN(m->function,FUNC_GLOBALS),0, o1);
          K(*dysym)() =(K(*)()) LOOK_(pair,0, o2)->i;

          I u = frame_height(m);

          if(u != m->function->nf) ERROR(ERROR_VALENCE);
          assert(u < DYNAMIC_FUNCTION_MAX_ARGS);

          K a[DYNAMIC_FUNCTION_MAX_ARGS] = {0};

          DO(u, a[i] = kN(m->stack, m->local_pointer - (1+i)));

          assert(DYNAMIC_FUNCTION_MAX_ARGS <= 8); // or change below by adding a[i]

          K z = ((K(*)())dysym)(a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7]);

          //We can
          //1. copy (/deep copy) this thing
          //2. We can free() it
          //3. etc.

          //K r = copy(z); //free(z); //techniques for malloc() returns
          K r=z;

          if(!r) r = Kn();  //handle NULL returns from C

          DO(u, pop(m))
          push_ri(m, r, true); 
          rd(r); //otherwise leaks INT atoms and such that dylib returns

          break;
      }

      case GLOBAL_PUSH:
      {
          ensure_stack_height_relative(m, 1);
          m->instruction_pointer++;  
          v = read_compressed_integer(m);
          K0 o1,o2,o3,o4;
          w = LOOK_(kN(m->function,FUNC_GLOBALS),v, o1);
          x = (K)(UI)LOOK_(w,0,o2)->i;
          y =        LOOK_(w,1,o3);
          z =        LOOK_(x,yi,o4);
          push_ri(m,z,true);
          break;
      }

      case GLOBAL_ALTER:
      {
          m->instruction_pointer++;  
          v = read_compressed_integer(m);

          K0 o1,o2,o3;
          K pair = LOOK_(kN(m->function,FUNC_GLOBALS),v,o1);
          K cmap = (K)(UI)LOOK_(pair,0,o2)->i;
          I p    =        LOOK_(pair,1,o3)->i;

          K values = kN(cmap,VALUES);
          assert(CAN_WRITE(cmap));
          //Serious Talk:
          //(In reference to creating a "values(map)" verb:)
          //We can't reference increment the VALUES LIST of a compile-referenced map
          //because then we would have to cow it during global alters
          //but that would be bad because it would force copy all our c-ref'd submaps
          //this blows up the universe (subsequent writes would fail)
          //Probably though that can't happen since:
          //if the only way we can get to this function is by pushing a cref'd map
          //onto the stack and ref incrementing it, then we're OK since that
          //triggers a special safe deep copy. So if that's true
          //we're in the clear here
          //I don't think values(K x)is a real verb anyway. d[] is values
          //
          //fairly sure the following is enforced since to get to the "raw" values
          //in the [compile referenced] map, you need to push strong(map) onto the stack
          //hence it triggers the safe cref'd-map-copy() instead
          //which will handle the values thing
          assert(CAN_WRITE(values));
          //this would not work -> nestneu(cmap, VALUES, values=cow(values));

          //.[w;x;y;z]
          //w = LOOK_(cmap,p);
          u = frame_height(m);
          
          x=NULL;
          y=NULL;
          z=NULL;


          K0 o4,o5,o6,o7,o8;
          x = LOOK_(m->stack, m->local_pointer-1,o4);
          y = kN(m->stack, m->local_pointer-2); 
          y = IS_EMPTY_ATOM(y) ? NULL : LOOK_(m->stack, m->local_pointer-2, o5);
          if(u>=3)
          {
            z = LOOK_(m->stack, m->local_pointer-3,o6);
          }

          assert(IS_LIST(values));//never demote cref
          //In order to make in-place updates work correctly
          //we need the tree MAP's VALUES to aways be a LIST

          K old = LOOK_(values, p,o7);

          cow_change(values,ki(p),x,y,z,false);
          //if it isn't we have to use the slower hash method
          //cow_change(cmap, LOOK_(kN(cmap,KEYS),p), x, y, z);
          //compare also
          //nestneu(cmap, VALUES, cow_change(values,ki(p),x,y,z)));
          w = LOOK_(values, p, o8);

          DO(u, pop(m))
          push_ri(m, w, true);
          The_Last_Expression_Was_Assignment_Flag = 1;
          break; 
      }

      case SQL_SELECT2:{
          m->instruction_pointer++;

          K0 o1,o2,o3,o4,o5;

            x = LOOK_(m->stack, m->local_pointer-1, o1);
            y = LOOK_(m->stack, m->local_pointer-2, o2); 
            z = LOOK_(m->stack, m->local_pointer-3, o3); 
          K a = LOOK_(m->stack, m->local_pointer-4, o4);
          K b = LOOK_(m->stack, m->local_pointer-5, o5);

          w = placeholder_sql_select(x,y,z,a,b,0,0);

          DO(5, pop(m))
          push_ri(m, w, false);

          break;
      }

      case SQL_UPDATE2:{
          m->instruction_pointer++;

          K0 o1,o2,o3,o4,o5;

            x = LOOK_(m->stack, m->local_pointer-1, o1);
            y = LOOK_(m->stack, m->local_pointer-2, o2); 
            z = LOOK_(m->stack, m->local_pointer-3, o3); 
          K a = LOOK_(m->stack, m->local_pointer-4, o4);
          K b = LOOK_(m->stack, m->local_pointer-5, o5);

          w = placeholder_sql_update(x,y,z,a,b,0,0);

          DO(5, pop(m))
          push_ri(m, w, false);

          break;
      }

      case SQL_DELETE2:{
          m->instruction_pointer++;

          K0 o1,o2,o3;
            x = LOOK_(m->stack, m->local_pointer-1, o1);//table
            y = LOOK_(m->stack, m->local_pointer-2, o2);//where_clauses 
            z = LOOK_(m->stack, m->local_pointer-3, o3);//column_clauses

          w = placeholder_sql_delete(x,y,z,0,0);

          DO(3, pop(m))
          push_ri(m, w, false);

          break;
      }

      case SQL_INSERT2:{
          m->instruction_pointer++;
            
          K a = NULL;
          
          K0 o1,o2,o3,o4;

          x = LOOK_(m->stack, m->local_pointer-1, o1);

          y = kN(m->stack,   m->local_pointer-2); 
          y = IS_EMPTY_ATOM(y) ? NULL : LOOK_(m->stack, m->local_pointer-2, o2);

          z = LOOK_(m->stack, m->local_pointer-3, o3);

          a = kN(m->stack,   m->local_pointer-4); 
          a = IS_EMPTY_ATOM(a) ? NULL : LOOK_(m->stack, m->local_pointer-4, o4);

          //Target, Columns, Values, Conflict_Keys
          w = placeholder_sql_insert(x, y, z, a);

          DO(4, pop(m))
          push_ri(m, w, false);

          break;
      }

      case SQL_PEEK_NAME:{
          m->instruction_pointer++;
          v = read_uncompressed_integer(m);

          x = NULL;
          if(0<frame_height(m)){K0 o; x = peeko(m,0,o); }
          if(x && !IS_NIL(x))
          { 
            K0 o;
            K constants = kN(m->function, FUNC_CONSTANTS);
            m->sql_peek = LOOK_(constants, v,o);
          }
          else
          {
          
          }

          break;
      }

      case SQL_TABLE_LOOKUP:{
          m->instruction_pointer++;

          K0 o1,o2;
          x = LOOK_(m->stack, m->local_pointer-1, o1);
          y = LOOK_(m->stack, m->local_pointer-2, o2); 

          bool is_where_clause = GET_ALT_ATTR(m->function, FUNC_ATTR_SQL_WHERE);
          bool is_pre_whered   = GET_ALT_ATTR(m->function, FUNC_ATTR_PRE_WHERED);

          if(IS_ATLAS(x) )
          {
            K w = NULL;

            if(is_where_clause) //WHERE
            {
              if(is_pre_whered)
              {

              }

              //see `is_special_atlas_form`
              w = new_k(LIST, 2);
              nestset_ri_rd(w, 0, y, true, false);
              nestset_ri_rd(w, 1, x, true, false);

              DO(2, pop(m))

              push_ri(m, w, false);
            }
            else //GROUP_BY, SELECT, etc.
            {

              //Note: so far this looks pretty similar to TABLE ver. below
              //      now it doesn't because we're using `at` != `AT`
              w = at(x,y);

              DO(2, pop(m))

              push_ri(m, w, false);
            }

          }
          else //TABLE
          {
            K0 o;
            w = AT2(x,y,o);
            DO(2, pop(m))

            if(IS_DISK(y) && IS_TENANT(y))
            {
              push_ri(m, w, false);
            }
            else
            {
              //this is the janky way to push an i on the stack
              //however "i<2" is not very fast if you have a 
              //multi-billion row table. Solving this is more
              //involved. Maybe we would go with this anyway,
              //maybe not.
              //You could certainly set up the comparison 
              //functions with a bogus range object, as we did
              //for atlas
              //Another weird idea... you could create a type
              //that was O(1) size, but always returned 
              //the value of the index when indexed. I guess
              //{[x] x} does this?
              if(false && IS_NIL(w) && matchC(y,kcv("i"),0))
              {
                w = til(ki(table_rows(x)));
                push_ri(m, w, false);
              }
              else 
              {
                push_ri(m, w, true);
              }
            }

          }

          break;
      }

      case ARGLOCAL_PUSH:
          m->instruction_pointer++;
          v = read_compressed_integer(m);
          push_arglocal(m, v);
          break;

      case ARGLOCAL_ALTER:
      {
          m->instruction_pointer++;  
          v = read_compressed_integer(m);

          I var_index = m->function_bottom + v;

          //.[w;x;y;z]
          //w = LOOK_(m->stack, var_index);

          u = frame_height(m);

          x=NULL;
          y=NULL;
          z=NULL;

          K0 o1,o2,o3,o4;
          x = LOOK_(m->stack, m->local_pointer-1, o1);
          y = kN(m->stack, m->local_pointer-2); 
          y = IS_EMPTY_ATOM(y) ? NULL : LOOK_(m->stack, m->local_pointer-2, o2);
          if(u>=3) z = LOOK_(m->stack, m->local_pointer-3 ,o3);

          cow_change(m->stack, ki(var_index), x, y, z, false);

          w = LOOK_(m->stack, var_index, o4);

          DO(u, pop(m))
          push_ri(m, w, true);
          The_Last_Expression_Was_Assignment_Flag = 1;
          break;

      }
      case SELF_PUSH:
          ensure_stack_height_relative(m, 1);
          m->instruction_pointer++;  

          K function = new_func_closed_from_stack(m->function, m);

          push_ri(m, function, false);
          break;

      case ASIDE_OPEN:
          m->instruction_pointer++;  
          frame_open(m);
          break;
          
      case ASIDE_CLOSE:
          m->instruction_pointer++;  
          //Empty brackets return NIL
          //Otherwse return last argument
          //But that functionality is probably/usually unreachable
          //What we want from (1;2;3) is just 3
          x = kn;
          u = frame_height(m);
          if(u > 0){K0 o; x = peeko(m,0,o); scrub(m);}
          frame_close(m);
          push_ri(m, x, false);
          break;

      case LIST_OPEN:
      {
          m->instruction_pointer++;  
          push_ri(m, new_k(LIST,0 ), false);
          frame_open(m);
          break;
      }

      case LIST_CLOSE:
      {
          m->instruction_pointer++;  
          frame_close(m);
          break;
      }

      case LIST_STORE:
      {
          m->instruction_pointer++;  

          I pos = m->local_bottom - (1+1);
          K0 o;
          K peek = peeko(m,0,o);
          K accum = kN(m->stack, pos);
          accum = cow_add_funny(accum, peek); //cow_add_funny for -FLOAT [1,2,4/6]
          nestset_ri_rd(m->stack, pos, accum, false, false);
          pop(m);

          break;
      }

      case ARG_OPEN:
          m->instruction_pointer++;  
          frame_open(m);
          break;

      case ARG_CLOSE:
          m->instruction_pointer++;  
          x = new_k(LIST, 0);
          u = frame_height(m);
          DO(u, K0 o; K y = LOOK_(m->stack, m->local_bottom + i, o); if(0==cy)y=kn; x = cow_join(x, y))
          frame_close(m);
          push_ri(m, x, false);
          break;

      case ADVERB_OPEN:
      {
          //We need open at the beginning, otherwise there's
          //no way sentinel could exist in the stack
          //Also, this set the "preset" valence via bytecode
          //whether it's sentinel or not
          //
          //We cannot handle all of the init stuff here since we don't
          //know yet whether we'll be pushing a variable-function
          //and if so what valence it has
          //Similarly, we need to adverb_open here so a function is distinguishable
          //if we do push it on the stack
          adverb_open(m);

          break;
      }

      case ADVERB_CLOSE: //technically, we could fold this into each of the ADVERB_WHILE loops' ends
      {
          adverb_close(m);
          break;
      }

      case ADVERB_STORE: //claim result, increment counter, record waypoints
      {
          //POTENTIAL_OPTIMIZATION_POINT: you could have a combined store+jmp instruction
          m->instruction_pointer++;  

          K0 o;
          K peek = peeko(m,0,o);
          ADVERB_SET(m, ADVERB_LATEST, peek, true, true);
          pop(m);

          adverb_increment_i(m);

          break;
      }

      case ADVERB_FOLD_WHILE:
      {
          m->instruction_pointer++;  
          v = read_compressed_integer(m);
          u = m->instruction_pointer;

          bool clause = true;

          I i, n, op_valence, subkind;
          K previous, latest, desired;
over_while_reswitch: //assignment under this label, at least, because the init updates it
          i = ADVERB_GET(m, ADVERB_i)->i;
          n = ADVERB_GET(m, ADVERB_n)->i;
          op_valence = ADVERB_GET(m, ADVERB_WORKING_VALENCE)->i;
          subkind = ADVERB_GET(m, ADVERB_SUBKIND)->i;

          previous = ADVERB_GET(m, ADVERB_PREVIOUS);
          latest = ADVERB_GET(m, ADVERB_LATEST);

          SW(op_valence)
          {
            CS(SENTINEL, adverb_init(m, ADVERB_KIND_FOLD); goto over_while_reswitch;)//First visit
           CSF(NILADIC,) 
            CS(MONADIC, 
                SW(subkind)
                {
                  CD:
                  CS(SUBKIND_REGULAR, if(i!=0 && adverb_is_dupe(m))clause=false; desired = latest;) //note: `previous` gets freed below
                  CS(SUBKIND_DO_N,    if(i>=n) clause=false; desired = latest;)
                  CS(SUBKIND_WHILE_B, K0 o; K f = ADVERB_GET_ARG_(m,0,o); //copy-pasta with SCAN
                                      K arg = latest;

                                      K z = NULL;

                                      SW(f->t)
                                      {
                                        CS(FUNC, z = _MONAD_EX(false, f, arg))
                                        CD: z = of_i_w(f,arg,0,0,false);
                                      }

                                      if(falsy(z)) clause = false;
                                      rd(z);
                                      desired = latest;

                    )
                }
              ) 
            CD: if(i>=n)clause = false;
                desired = latest;
                break;
          }

          ADVERB_SET(m, ADVERB_ACCUMULATOR, desired, true, true);
          ADVERB_SET(m, ADVERB_PREVIOUS, latest, true, true);

          if(!clause) m->instruction_pointer = u + v;

          break;
      }

      case ADVERB_UNFOLD_WHILE: //You can factor this with ADVERB_FOLD_WHILE but honestly it's just more headache
      {
          m->instruction_pointer++;  
          v = read_compressed_integer(m);
          u = m->instruction_pointer;

          bool clause = true;

          I i, n, op_valence, subkind;
          K previous, latest, desired = NULL;
          
scan_while_reswitch: //assignment under this label, at least, because the init updates it
          i = ADVERB_GET(m, ADVERB_i)->i;
          n = ADVERB_GET(m, ADVERB_n)->i;
          op_valence = ADVERB_GET(m, ADVERB_WORKING_VALENCE)->i;
          subkind = ADVERB_GET(m, ADVERB_SUBKIND)->i;

          previous = ADVERB_GET(m, ADVERB_PREVIOUS);
          latest = ADVERB_GET(m, ADVERB_LATEST);

          SW(op_valence)
          {
            CS(SENTINEL, adverb_init(m, ADVERB_KIND_UNFOLD); goto scan_while_reswitch;)//First visit
           CSF(NILADIC,) CS(MONADIC, 
                SW(subkind)
                {
                  CD:
                  CS(SUBKIND_REGULAR, desired = latest; //note: previous gets freed below
                                      if(i!=0 && adverb_is_dupe(m))
                                      {
                                        clause = false; 
                                        desired = NULL;
                                      }
                    ) 
                  CS(SUBKIND_DO_N, if(i>=n) clause=false; desired = latest;)
                  CS(SUBKIND_WHILE_B, K0 o; K f = ADVERB_GET_ARG_(m,0,o); //copy-pasta with OVER
                                      K arg = latest;
                                      K z = _MONAD_EX(false, f,arg);
                                      if(falsy(z)) clause = false;
                                      rd(z);
                                      desired = latest;
                    )
                }
              ) 
            CD: if(i>=n)clause = false;
                if(i>0) //so that triadic scan {[x,y,z] }\\ works better
                desired = latest;
                break;
          }

          ADVERB_SET(m, ADVERB_PREVIOUS, latest, true, true);

          if(desired)
          {
            K accum = ADVERB_GET(m, ADVERB_ACCUMULATOR);
            accum = cow_add(accum, desired);
            ADVERB_SET(m, ADVERB_ACCUMULATOR, accum, false, false);
          }

          if(!clause)
          {
            m->instruction_pointer = u + v;
          }

          break;
      }

      case ADVERB_UNFOLD_PUSH:
      case ADVERB_FOLD_PUSH:
      {
          m->instruction_pointer++;  

          I op_valence  = ADVERB_GET(m, ADVERB_WORKING_VALENCE)->i;
          I argc        = ADVERB_GET(m, ADVERB_SUPPLIED_ARGS)->i;
          I index       = ADVERB_GET(m, ADVERB_i)->i;
          I pushing = 0;
          I start = 0;

          SW(op_valence)
          {
           CSF(NILADIC,) CS(MONADIC, )//noop
            CD: 
              //push is based on args supplied not valence of operator 

              SW(argc)// {x+y}/b versus a {x+y}/b
              {
                CS(1, start = 0; pushing = 1;)
                CD:   start = 1; pushing = argc - 1; break;
              }
      
              ensure_stack_height_relative(m, pushing);
              DO2(pushing, K0 o1,o2; K x = ADVERB_GET_ARG_(m, start + j,o1); K y = LOOK_(x, index, o2); push_ri(m, y, true))
              break;
          }
         
          ensure_stack_height_relative(m, 1);
          push_ri(m, ADVERB_GET(m, ADVERB_LATEST), true);

          break;
      }

      case ADVERB_MAPLEFT_WHILE:
      case ADVERB_MAPRIGHT_WHILE:
      case ADVERB_MAPDOWN_WHILE:
      {
          m->instruction_pointer++;  
          v = read_compressed_integer(m);
          u = m->instruction_pointer;

          bool clause = true;

          I i, n, op_valence;
          K latest, desired = NULL;
          
each_while_reswitch: //assignment under this label, at least, because the init updates it
          i = ADVERB_GET(m, ADVERB_i)->i;
          n = ADVERB_GET(m, ADVERB_n)->i;
          op_valence = ADVERB_GET(m, ADVERB_WORKING_VALENCE)->i;

          latest = ADVERB_GET(m, ADVERB_LATEST);

          SW(op_valence)
          {
            CS(SENTINEL, 
                SW(inst)
                {
                  CS(ADVERB_MAPLEFT_WHILE,  adverb_init(m, ADVERB_KIND_MAPLEFT))
                  CS(ADVERB_MAPRIGHT_WHILE, adverb_init(m, ADVERB_KIND_MAPRIGHT))
                  CD:                       adverb_init(m, ADVERB_KIND_MAPDOWN);
                }
                goto each_while_reswitch;)//First visit
            CD: if(i>=n) clause = false;
                desired = latest;
                break;
          }

          if(i>0)
          {
            K accum = ADVERB_GET(m, ADVERB_ACCUMULATOR);
            ADVERB_SET(m, ADVERB_ACCUMULATOR, cow_add(accum, desired), false, false);
          }

          if(!clause)
          {
            //partial work on making "0 join mapleft 1 2" not be extra enlisted
            //which I don't even know if we want
            //I argc = ADVERB_GET(m, ADVERB_SUPPLIED_ARGS)->i;

            //K first_arg = ADVERB_GET_ARG_(m, 0);
            //K final_arg = ADVERB_GET_ARG_(m, argc - 1);

            //SW(inst)
            //{
            //  CS(ADVERB_MAPLEFT_WHILE, if(IS_ATOM()) )
            //  CS(ADVERB_MAPRIGHT_WHILE, )
            //  CD:                       
            //}
 
            m->instruction_pointer = u + v;
          }

          break;
      }

      case ADVERB_MAPDOWN_PUSH:
      {
          m->instruction_pointer++;  

          I pushing = ADVERB_GET(m, ADVERB_SUPPLIED_ARGS)->i;
          I index   = ADVERB_GET(m, ADVERB_i)->i;

          ensure_stack_height_relative(m, pushing);
          DO2(pushing, K0 o,o2; K x = ADVERB_GET_ARG_(m, pushing -(j+1),o); K y = LOOK_(x, index, o2); push_ri(m, y, true))

          break;
      }

      case ADVERB_MAPRIGHT_PUSH:
      {
          m->instruction_pointer++;  

          I pushing = ADVERB_GET(m, ADVERB_SUPPLIED_ARGS)->i;
          I index   = ADVERB_GET(m, ADVERB_i)->i;

          ensure_stack_height_relative(m, pushing);
          assert(pushing >= 1);

          K0 o,o2;
          K x = ADVERB_GET_ARG_(m, pushing - 1,o);
          K y = LOOK_(x, index, o2); 
          push_ri(m, y, true);
          DO2(pushing - 1, K0 o; K y = ADVERB_GET_ARG_(m, pushing - (j+2),o);  push_ri(m, y, true))

          break;
      }

      case ADVERB_MAPLEFT_PUSH:
      {
          m->instruction_pointer++;  

          I pushing = ADVERB_GET(m, ADVERB_SUPPLIED_ARGS)->i;
          I index   = ADVERB_GET(m, ADVERB_i)->i;

          ensure_stack_height_relative(m, pushing);
          assert(pushing >= 1);

          DO2(pushing - 1, K0 o; K y = ADVERB_GET_ARG_(m, pushing - (j+1),o); push_ri(m, y, true))
          K0 o, o2;
          K x = ADVERB_GET_ARG_(m, 0, o);
          K y = LOOK_(x, index,o2); 
          push_ri(m, y, true);

          break;
      }

      case ADVERB_MAPBACK_WHILE:
      {
          m->instruction_pointer++;  
          v = read_compressed_integer(m);
          u = m->instruction_pointer;

          bool clause = true;

          bool mapback_parent = false;

          I i, n, op_valence, supplied_args;
          K latest, desired = NULL;

          I spawn = 0;
          
mapback_while_reswitch: //assignment under this label, at least, because the init updates it
          i = ADVERB_GET(m, ADVERB_i)->i;
          n = ADVERB_GET(m, ADVERB_n)->i;
          op_valence    = ADVERB_GET(m, ADVERB_WORKING_VALENCE)->i;
          supplied_args = ADVERB_GET(m, ADVERB_SUPPLIED_ARGS)->i;

          latest = ADVERB_GET(m, ADVERB_LATEST);

          SW(op_valence)
          {
            CS(SENTINEL, adverb_init(m, ADVERB_KIND_MAPBACK); goto mapback_while_reswitch;)//First visit
           CSF(NILADIC,) CS(MONADIC, 

              if(i==0)//first time through, fork
              {
                if(The_Process_is_at_Depth >= PARALLEL_EXECUTION_DEPTH_LIMIT)
                {
                  
                  close(The_Process_Shm_Handle);
                  _exit(EXIT_FAILURE);
                }

                spawn = total_cores();

                I j = 0;

                The_Process_is_Active_Parent = true;
                The_Process_Created_Registry_Bytes = MIN(PAGE_SIZE_BYTES, spawn * sizeof(K));
                bool shared = true;
                The_Process_Created_Registry = pool_anonymous_system_memory(The_Process_Created_Registry_Bytes, shared); //fine to leak this on error

                assert(spawn > 0);

                DO(spawn, The_Process_Shm_Handles[i] = 0)

                I bonus     = 0;
                I remainder = n % spawn;
                I generic   = (n / spawn) + (bonus ? 1 : 0);
                I curstart  = 0;
                I window    = 0;

                //POTENTIAL_OPTIMIZATION_POINT
                //`group by` (x2) has a mapright we can replace with mapcores
                //matrix mult has a mapleft we might be able to get

                //POTENTIAL_OPTIMIZATION_POINT
                //You can do parallel versions of mapdown, mapleft, mapright, (mapback - trickier)
                //(potentially using mapcores - maybe it would've been smarter to make adverbs "project" to grab verbs & such)
                //provided the argument is either a verb (non-assignment) or a noun that doesn't make assignments (functional)
                //if the noun is a function, you can check that the globals list is empty, or that no assignment instructions
                //are issued, (there is also the case of ."eval" which we can ignore), and that no subfunction has these (or no subfunctions).
                //potentially this a function-attribute which we enable when one of the above happens, for easy+fast checking.
                //You probably only want to trigger these parallel versions when the list is long enough to justify it.

                //POTENTIAL_OPTIMIZATION_POINT
                //for speedups(?) see: (osx)   http://yyshen.github.io/2015/01/18/binding_threads_to_cores_osx.html
                // (cpu affinity)      (linux) http://man7.org/linux/man-pages/man3/pthread_setaffinity_np.3.html

                //POTENTIAL_OPTIMIZATION_POINT
                //see the new memfd_create for a possible improvement

                bool flag = true;
                for(j = 0; j < spawn && flag; j++)
                {
                  bonus = (remainder > j) ? 1 : 0;
                  curstart += window;
                  window =  generic + bonus;

                  if(0==strlen(The_Process_Shm_Name))
                  {
                    snprintf(The_Process_Shm_Name, sizeof(The_Process_Shm_Name), "/kerf-%lld-core", (I)getpid()); //we could move this to init...
                  }
                  
                  char name_buf[256] = {0}; 
                  snprintf(name_buf, sizeof(name_buf), "%s-%lld", The_Process_Shm_Name, j);

                  int shm_fd;

                  shm_unlink(name_buf);
                  shm_fd = shm_open(name_buf, O_RDWR | O_CREAT, S_IRUSR | S_IWUSR);
                  shm_unlink(name_buf);//we can do this b/c handles will keep alive


                  if (-1 == shm_fd)
                  {
                    perror("Shared memory open failed");
                    ERROR(ERROR_PARALLEL);
                  }

                  SW(kerf_fork())
                  {
                    CS(-1,  ERROR(ERROR_PARALLEL))
                    CS( 0, 
                            //Children
                            The_Process_is_Child_Number    = j; 
                            The_Process_Family_Size        = spawn;
                            The_Process_Inherited_Registry = The_Process_Created_Registry;
                            The_Process_Created_Registry   = NULL;
                            clause                         = true;
                            mapback_parent                 = false;
                            flag                           = false;
                            snprintf(The_Process_Shm_Name, sizeof(The_Process_Shm_Name), "%s", name_buf);
                            The_Process_Segment_Starter    = curstart;
                            The_Process_Segment_Window     = window;
                            The_Process_Shm_Handle         = shm_fd;
                            DO(spawn, The_Process_Shm_Handles[i] = 0)
                            break;
                    ) 
                    CD:{   //Parent
                           The_Process_Shm_Handles[j] = shm_fd;
                           mapback_parent = true;
                           clause = false;
                           break;
                    }
                  }
                } 
              }

              if(!mapback_parent)
              {
                desired = latest;

                bool interleaved = The_Process_Executes_Interleaved;
                bool segmented   = !interleaved;

                if(interleaved)
                {
                  if((The_Process_Family_Size * i) + The_Process_is_Child_Number >= n) clause = false;
                }
                else
                {
                  if(The_Process_Segment_Starter + i >= The_Process_Segment_Starter + The_Process_Segment_Window) clause = false;
                }
              }
              else if(mapback_parent)
              {
                goto mapback_while_finish;
              }
            )
            CD: if(i>=n)clause = false;
                desired = latest;
                break;
          }

          if(op_valence >= 2 && i==0 && 1 == supplied_args) //force uniformity, so that list length is preserved 
          {
            K previous = ADVERB_GET(m, ADVERB_PREVIOUS);
            K accum = ADVERB_GET(m, ADVERB_ACCUMULATOR);
            ADVERB_SET(m, ADVERB_ACCUMULATOR, cow_join(accum, previous), false, false);
          }
          else if(i>0)
          {
            K accum = ADVERB_GET(m, ADVERB_ACCUMULATOR);
            ADVERB_SET(m, ADVERB_ACCUMULATOR, cow_add(accum, desired), false, false);
          }

mapback_while_finish:

          if(!clause)
          {
            if(op_valence <= 1)
            {
              if(!mapback_parent)
              {
                K source = ADVERB_GET(m, ADVERB_ACCUMULATOR);

                I bytes = wire_size_K(source);

                int t = ftruncate(The_Process_Shm_Handle, bytes);
                if(t<0)
                {
                  perror("Bad truncate on shared memory handle");
                  _exit(EXIT_FAILURE);
                }

                K shm_base = mmap(0, bytes, PROT_READ | PROT_WRITE, MAP_SHARED, The_Process_Shm_Handle, 0);
                if(MAP_FAILED == shm_base)
                {
                  perror("Shared-memory mmap failed");
                  _exit(EXIT_FAILURE);
                }
                shm_base->m = ceiling_log_2(bytes);

                //POTENTIAL_OPTIMIZATION_POINT
                //see note at this function. it's slow
                copy_from_k_to_good_disk_membuf_temp(shm_base, source);

                munmap(shm_base, bytes);
                close(The_Process_Shm_Handle);
                The_Process_Shm_Handle = 0;

                The_Process_Inherited_Registry[The_Process_is_Child_Number] = bytes;
                //The_Process_Inherited_Registry[The_Process_is_Child_Number] = shared;
                //The_Process_Inherited_Registry[The_Process_is_Child_Number] = The_Process_is_Child_Number;

                _exit(EXIT_SUCCESS);
              }
              else if(mapback_parent)
              {
                //TODO: to accurately time parallel map, you'll need wall clock time

                //POTENTIAL_OPTIMIZATION_POINT
                //we can actually get started on reassembly prior to all spawns finishing


                DO2(spawn, pid_t pid = wait(NULL))

                bool error_flag = false;

                K list = new_k(LIST, n/2); list->n = 0;
                work_push(list);

                DO(spawn,

                    I bytes = The_Process_Created_Registry[i];

                    if(bytes <= 0)
                    {
                      //fprintf(stderr, "Missing byte count for parallel return.\n");
                      error_flag = true;
                      break;
                    }

                    int shm_fd = The_Process_Shm_Handles[i];

                    V shm_base = mmap(0, bytes, PROT_READ | PROT_WRITE, MAP_SHARED, shm_fd, 0);

                    if (shm_base == MAP_FAILED) {
                      perror("Mapped shared filehandle failed");
                      error_flag = true;
                      break;
                    }

                    //POTENTIAL_OPTIMIZATION_POINT
                    //could be 2x faster to use interleaved? assuming reassembly doesn't kill you
                    //you can use existing arch. just update the LIST in place instead of joining. maybe demote

                    //POTENTIAL_OPTIMIZATION_POINT
                    //we can reassemble interleaved and/or multithreaded
                    //(keep all mapped at once, go down each one.)
                    //may or may not help

                    work_pop();
                    list = cow_join(list, shm_base);
                    work_push(list);

                    munmap(shm_base, bytes);
                    close(shm_fd);
                    The_Process_Shm_Handles[i] = 0;
                    //optional: shm_unlink
                )

                munmap(The_Process_Created_Registry, The_Process_Created_Registry_Bytes);
                The_Process_Created_Registry = false;

                if(error_flag)
                {
                  DO(spawn, close(The_Process_Shm_Handles[i]))
                  ERROR(ERROR_PARALLEL);
                }

                The_Process_is_Active_Parent = false;

                ADVERB_SET(m, ADVERB_ACCUMULATOR, list, true, true);
                work_pop_rd(true);
              }
            }

            m->instruction_pointer = u + v;
          }

          break;
      }

      case ADVERB_MAPBACK_PUSH:
      {
        m->instruction_pointer++;  

          I op_valence  = ADVERB_GET(m, ADVERB_WORKING_VALENCE)->i;
          I argc        = ADVERB_GET(m, ADVERB_SUPPLIED_ARGS)->i;
          I index       = ADVERB_GET(m, ADVERB_i)->i;
          I pushing = 0;
          I start = 0;
          I offset = 0;

          SW(op_valence)
          {
           CSF(NILADIC,) CS(MONADIC, 
              //Copy-paste from mapdown push

              I pushing = argc;

              bool interleaved = The_Process_Executes_Interleaved;
              bool segmented   = !interleaved;

              I pindex;
              
              if(interleaved)
              {
                pindex = (The_Process_Family_Size * index) + The_Process_is_Child_Number;
              }
              else
              {
                pindex = The_Process_Segment_Starter + index; 
              }

              ensure_stack_height_relative(m, pushing);
              DO2(pushing, K0 o, o2; K x = ADVERB_GET_ARG_(m, pushing -(j+1),o); K y = LOOK_(x, pindex, o2); push_ri(m, y, true))
            ) 
            CS(DYADIC, 
                pushing = 2;
                ensure_stack_height_relative(m, pushing);
                SW(argc)// {x+y}/b versus a {x+y}/b
                {
                  CS(1, start = 0; offset = 1;)
                  CS(2, start = 1; offset = 0;)
                }

                push_ri(m, ADVERB_GET(m, ADVERB_PREVIOUS), true);
                K0 o,o2; 
                K x = ADVERB_GET_ARG_(m, start + 0, o); 
                K y = LOOK_(x, index + offset, o2); 
                ADVERB_SET(m, ADVERB_PREVIOUS, y, true, true);
                push_ri(m, y, true);
            )
            CD: ERROR(ERROR_ARITY); break; //eachpair[a;b;c] error - don't even know how to do this
          }
         
          break;
      }

      case APPLY_NOPROJECT: //TODO? OMIT?
      case APPLY:

          m->instruction_pointer++;  
          K0 o;
          K x = peeko(m,0,o);

          SW(xt)
          {
            CS(FUNC, function_apply(m))
            CD: 
            {
              K y = kk;
              u = frame_height(m);
              assert(u >= 1);

              bool reverso = true;
              if(reverso)
              {
                DO(u - 1, K0 o; y = cow_add(y, LOOK_(m->stack, m->local_bottom + u + -i + -2, o)))
              }
              else
              {
                DO(u - 1, K0 o; y = cow_add(y, LOOK_(m->stack, m->local_bottom + i, o)))
              }

              work_push(y);
              K z = of_i_w(x,y,0,0,false);
              work_pop_rd(true);
              while(m->local_pointer > m->local_bottom) pop(m);
              push_ri(m, z, true);
              rd(z);
              break;
            }
          }
          
          break;

      case VERB_CALL:
      {
          // x0. min/max args
          // x0. actual execution
          // x0. projections, if any
          // x0. early errors?
          // x0. avoid z memory leaks on error
          // x0. regular atomic
          // x0. string/byte_vec atomic
          // x0. map atomic
          //  0. claim stack memory

          //POTENTIAL_OPTIMIZATION_POINT
          //Claiming Stack Memory
          //One method for reclaiming stack memory would be for
          //verb methods to have some kind of property indicating
          //in the verb table what sort of reclamation is acceptable
          //(this is likely O(c) with c==4 in complexity). Then
          //verbs may use a generalization of new_k to grab
          //dying structures from this pool. For instance, there
          //are many cases when the verb "plus" can trivially
          //overwrite one of the provided arguments.
          //The generalized new_k would check the KVM registers,
          //or the stack, for dying objects (reference count 1?),
          //who are of a certain size. Then the verb would reference
          //increment the object, and safely overwrite it as it
          //reads from the object.
          //*Another technique: for objects small enough to fit in the
          //pool, we can simply put them in the pool and carry on as
          //if nothing happened.
          //*Another technique: we can use the VERB TABLE to mark
          //whether first, second, ... args can be reclaimed.
          //*Another technique: is it true that we can just capture
          //anything with refcount 1 on the stack frame, or is
          //there a counter-example? If it's true that would
          //make things very easy

          m->instruction_pointer++;  
          v = read_uncompressed_integer(m);

          I argc = frame_height(m);

          VERB verb = VERB_DISPATCH[v];

          I min_args = verb.argc_range[0];
          I max_args = verb.argc_range[1];

          if(argc < min_args || max_args < argc)
          {
            //show_stack(m);
            //dd(argc)
            //dd(min_args)
            //dd(max_args)
            ERROR(ERROR_VALENCE);
          }

          K a[MAX_SIMPLE_VERB_ARGS] = {NULL};

          //Look on the stack and fill our args array with the provided args
          K0 o1,o2,o3,o4;
          a[0] = peeko(m, 0, o1);
          a[1] = peeko(m, 1, o2);
          a[2] = peeko(m, 2, o3);
          a[3] = peeko(m, 3, o4);
          DO(MAX_SIMPLE_VERB_ARGS - argc, a[MAX_SIMPLE_VERB_ARGS - 1 - i] = NULL)
          //Technically this is a __HACK__, but it will work fine as long as 
          //VERB_CALL is always executed from within the context of a function,
          //which will ensure at least 4 args are on the stack (and technically
          //if they weren't we'd just be looking at m->stack's header which is OK)
          //
          //Bug: No idea why none of the following works correctly:
          //DO(argc, a[i] = peekn(m, i))
          //DO2(argc, dd(j) a[j] = LOOK_(m->stack, m->local_pointer-((j)+1)))
          //DO2(argc, a[j] = peekn(m, j))
          //Could be a macro/scope/inline issue? Compiling with -O0 did not solve.
          //Apple LLVM version 5.1 (clang-503.0.40) (based on LLVM 3.4svn)
          //Target: x86_64-apple-darwin13.4.0
          //Later on I found a bug that occured with -Os but not -O0 that
          //was solved by using `volatile` on my macros - possibly related.

          //fprintf(stderr, "verb: %s\n", verb.name);

          //assert: this handles each of 1, 2, ..., MAX_SIMPLE_VERB_ARGS

          //Type needs to be checked at ply or verb("plus") level, not VERB_CALL (would miss recusive)

          z = ply(a[0], a[1], a[2], a[3], argc, verb);

          //Aside: One unfortunate thing about using inline atoms is that the
          //items are now wider than the pointers themselves, and they're not just pointers
          //anymore; so you can't do the trick c_verb_call(arg1,2,3,4,5,6,7) that works
          //for verbs of any arity 8)

          DO(argc, pop(m))
          push_ri(m, z, true);
          rd(z);

          break;
      }

// HELPERS ////////////////////////////////////////////////////////////
      case JMPRFIF:
          m->instruction_pointer++;
          v = read_compressed_integer(m);

          x = NULL;
          if(0<frame_height(m)){K0 o; x = peeko(m,0,o); }
          if(x && xi > 1)
          { 
            m->instruction_pointer = m->instruction_pointer + v;
          }
          break;

      case SHOW:
          m->instruction_pointer++;
          //x = LOOK_(m->stack,m->local_pointer-1);
          //x?show(x):show(kn);
          show_stack(m);
          break;

      case ATOM_I_PUSH:
          ensure_stack_height_relative(m, 1);
          m->instruction_pointer++;  
          v = read_compressed_integer(m);
          push_ri(m, ki(v), false);
          break;
    }

  }

  return 0;
}

void wrapper_for_return(VM m, I local_ex_floor, bool *done) 
{
  function_close(m);

  //This check handles additional calls to local_ex
  //from the middle of an execution
  if(m->local_bottom <= local_ex_floor)
  {
    *done = true;
  }

  //if(!m->function) { *done = true; } //O("RETURN-BREAK\n");
  //else { *done = true; } //O("RETURN-CONTINUE\n");
}

#pragma mark -
