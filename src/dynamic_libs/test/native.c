#include "../kerf_api.h"

KERF demo_append(KERF base) {
	KERF tail = kerf_api_new_int(54);
	KERF list = kerf_api_append(kerf_api_retain(base), tail);
	kerf_api_release(tail);
	return list;
}

KERF demo_dyad(KERF dyad) {
	KERF a1  = kerf_api_new_int(3);
	KERF a2  = kerf_api_new_int(5);
	KERF ret = kerf_api_call_dyad(dyad, a1, a2);
	kerf_api_release(a1);
	kerf_api_release(a2);
	return ret;
}

KERF demo_monad(KERF monad) {
	KERF arg = kerf_api_new_int(17);
	KERF ret = kerf_api_call_monad(monad, arg);
	kerf_api_release(arg);
	return ret;
}

KERF demo_nilad(KERF nilad) {
	KERF temp = kerf_api_call_nilad(nilad);
	kerf_api_release(temp);
	return kerf_api_call_nilad(nilad);
}

KERF demo_get(KERF list, KERF index) {
	KERF zero = kerf_api_new_int(0);
	KERF first = kerf_api_get(list, zero);
	kerf_api_release(zero);
	kerf_api_release(first);
	return kerf_api_get(list, index);
}

KERF demo_interpret() {
	KERF string = kerf_api_new_charvec("a: 1+2; 2*range a");
	KERF ret = kerf_api_interpret(string);
	kerf_api_release(string);
	return ret;
}

KERF demo_len(KERF x) {
	KERF list = kerf_api_new_list();
	KERF num  = kerf_api_new_int(kerf_api_len(x));
	list = kerf_api_append(list, num);
	kerf_api_release(num);
	return list;
}

KERF demo_new_kerf(KERF base, KERF count) {
	KERF ret = kerf_api_new_kerf(KERF_INTVEC, count->i);
	for(int x = 0; x < count->i; x++) {
		((int64_t*)(ret->g))[x] = (base->i) + x;
	}
	return ret;
}

KERF demo_new_stamp(KERF ns) {
	return kerf_api_new_stamp(ns->i);
}

KERF demo_set(KERF list, KERF index, KERF rep) {
	return kerf_api_set(kerf_api_retain(list), index, rep);
}

KERF demo_attr_sorted_get(KERF list) {
	return kerf_api_new_int((list->a & KERF_ATTR_SORTED) != 0);
}

KERF demo_attr_bytes_set(KERF charvec) {
	charvec = kerf_api_retain(charvec);
	charvec->a |= KERF_ATTR_BYTES;
	return charvec;
}

KERF demo_attr_disk_get(KERF x) {
	return kerf_api_new_int((x->a & KERF_ATTR_DISK) != 0);
}