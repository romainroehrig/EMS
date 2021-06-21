# -*- coding:UTF-8 -*-

import sys, os

rep_EMS = os.getenv('REP_EMS')

import numpy as np

import ctypes as ct
_solib = ct.CDLL(rep_EMS+"/UTIL/python/lfa8/lfa.so")


def readr(fin,varname,ndim):

    _solib.readr_.argtypes = [\
                              ct.c_char_p,\
                              ct.c_char_p,\
                              ct.POINTER(ct.c_long),\
                              np.ctypeslib.ndpointer(dtype=np.float32,ndim=1,flags='F_CONTIGUOUS'),\
                              ]

    _solib.readr_.restype  = None

    chp    = np.ndarray((ndim,),    dtype=np.float32, order='F')

    _solib.readr_(\
                  ct.create_string_buffer(np.str(fin).ljust(100)),\
                  ct.create_string_buffer(np.str(varname).ljust(100)),\
                  ct.byref(ct.c_long(ndim)),\
                  chp\
                  )

    return chp

def readi(fin,varname,ndim):

    _solib.readi_.argtypes = [\
                              ct.c_char_p,\
                              ct.c_char_p,\
                              ct.POINTER(ct.c_long),\
                              np.ctypeslib.ndpointer(dtype=np.int32,ndim=1,flags='F_CONTIGUOUS'),\
                              ]

    _solib.readi_.restype  = None

    chp    = np.ndarray((ndim,),    dtype=np.int32, order='F')

    _solib.readi_(\
                  ct.create_string_buffer(np.str(fin).ljust(100)),\
                  ct.create_string_buffer(np.str(varname).ljust(100)),\
                  ct.byref(ct.c_long(ndim)),\
                  chp\
                  )

    return chp

def iterate_readr(varname,ndim,tstep,nstep):

    _solib.itreadr_.argtypes = [\
                              ct.c_char_p,\
                              ct.POINTER(ct.c_long),\
                              ct.POINTER(ct.c_float),\
                              ct.POINTER(ct.c_long),\
                              np.ctypeslib.ndpointer(dtype=np.float32,ndim=2,flags='F_CONTIGUOUS'),\
                              ]

    _solib.itreadr_.restype  = None

    chp    = np.ndarray((nstep,ndim),    dtype=np.float32, order='F')

    _solib.itreadr_(\
                  ct.create_string_buffer(np.str(varname).ljust(100)),\
                  ct.byref(ct.c_long(ndim)),\
                  ct.byref(ct.c_float(tstep)),\
                  ct.byref(ct.c_long(nstep)),\
                  chp\
                  )

    return chp

def iterate_readi(varname,ndim,tstep,nstep):

    _solib.itreadi_.argtypes = [\
                              ct.c_char_p,\
                              ct.POINTER(ct.c_long),\
                              ct.POINTER(ct.c_float),\
                              ct.POINTER(ct.c_long),\
                              np.ctypeslib.ndpointer(dtype=np.int32,ndim=2,flags='F_CONTIGUOUS'),\
                              ]

    _solib.itreadi_.restype  = None

    chp    = np.ndarray((nstep,ndim),    dtype=np.int32, order='F')

    _solib.itreadi_(\
                  ct.create_string_buffer(np.str(varname).ljust(100)),\
                  ct.byref(ct.c_long(ndim)),\
                  ct.byref(ct.c_float(tstep)),\
                  ct.byref(ct.c_long(nstep)),\
                  chp\
                  )

    return chp
