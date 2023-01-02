#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Copyright (c) Météo France (2014-)
# This software is governed by the CeCILL-C license under French law.
# http://www.cecill.info

import numpy

def lfareader(lib):
    """
    Returns a LFA reader object
    :param lib: None to use epygram or
                a path to a shared lib containing the following entries:
                - wlfaouv / wlfafer
                - wlfalecr / wlfaleci
                - wlfacas
                - wlfalaft
    Note: the shared lib can be compiled with EMS source code
          or compiled with gmkpack (-p libs4py)
          or taken in the epygram directory
    """
    if lib is None:
        import epygram
        epygram.init_env()
        class reader():
            def __init__(self, filename):
                self.r = epygram.formats.resource(filename, 'r', fmt='LFA')
            def listfields(self):
                return self.r.listfields()
            def readfield(self, field):
                return self.r.readfield(field).data
            def close(self):
                return self.r.close()
            def __enter__(self):
                return self
            def __exit__(self, exc_type, exc_value, traceback):
                return self.close()
    else:
        import ctypes
        _solib = ctypes.CDLL(lib)
        class reader():
            def __init__(self, filename):
                returncode, unit = ctypes.c_longlong(), ctypes.c_longlong()
                _solib.wlfaouv_.argtypes = [ctypes.POINTER(ctypes.c_longlong), #status
                                            ctypes.c_char_p, ctypes.c_char_p, #filename, open mode
                                            ctypes.POINTER(ctypes.c_longlong), #unit
                                            ctypes.c_longlong, ctypes.c_longlong] #string lengths (by value)
                _solib.wlfaouv_(ctypes.byref(returncode),
                                ctypes.create_string_buffer(filename.encode("utf-8")),
                                ctypes.create_string_buffer('R'.encode("utf-8")),
                                ctypes.byref(unit),
                                ctypes.c_longlong(len(filename.encode("utf-8"))),
                                ctypes.c_longlong(1))
                assert returncode.value == 0, "Error in opening LFA file " + filename
                self._unit = unit.value
                self.LFA_max_num_fields = 1000
                self.LFA_maxstrlen = 200
                self.filename= filename
            def listfields(self):
                listfields = numpy.ndarray((self.LFA_max_num_fields,), dtype=('S', self.LFA_maxstrlen), order='F')
                listfields = numpy.asfortranarray(numpy.core.defchararray.ljust(listfields, self.LFA_maxstrlen))
                fieldsnum = ctypes.c_longlong()
                _solib.wlfalaft_.argtypes = [ctypes.POINTER(ctypes.c_longlong), #logical unit
                                             ctypes.POINTER(ctypes.c_longlong), #maximum number of fields
                                             ctypes.POINTER(ctypes.c_longlong), #maximum length of field name
                                             ctypes.POINTER(ctypes.c_longlong), #number of fields
                                             numpy.ctypeslib.ndpointer(dtype=('S', self.LFA_maxstrlen),
                                                                       ndim=1,
                                                                       flags=str('F_CONTIGUOUS'))]  # Note: str() needed in Python2 for unicode/str obscure inner incompatibility
                _solib.wlfalaft_(ctypes.byref(ctypes.c_longlong(self._unit)),
                                 ctypes.byref(ctypes.c_longlong(self.LFA_max_num_fields)),
                                 ctypes.byref(ctypes.c_longlong(self.LFA_maxstrlen)),
                                 ctypes.byref(fieldsnum),
                                 listfields)
                return [listfields[i].strip().decode() for i in range(fieldsnum.value)]
            def readfield(self, fieldname):
                (fieldtype, fieldlength) = self._wlfacas(fieldname)
                if fieldtype[0] == 'R':
                    (data, fieldlength) = self._wlfalecr(fieldname, fieldlength)
                elif fieldtype[0] == 'I':
                    (data, fieldlength) = self._wlfaleci(fieldname, fieldlength)
                elif fieldtype[0] == 'C':
                    (data, fieldlength) = self._wlfalecc(fieldname, fieldlength)
                    data = [data[i].strip().decode() for i in range(fieldlength)]
                return data
            def _wlfacas(self, fieldname):
                returncode, length = ctypes.c_longlong(), ctypes.c_longlong()
                articletype = ctypes.create_string_buffer(2)
                _solib.wlfacas_.argtypes = [ctypes.POINTER(ctypes.c_longlong), #status
                                            ctypes.POINTER(ctypes.c_longlong), #logical unit
                                            ctypes.c_char_p, #article name
                                            ctypes.c_char_p, #article type
                                            ctypes.POINTER(ctypes.c_longlong), #article length
                                            ctypes.c_longlong, ctypes.c_longlong] #string lengths (by value)
                _solib.wlfacas_(ctypes.byref(returncode),
                                ctypes.byref(ctypes.c_longlong(self._unit)),
                                ctypes.create_string_buffer(fieldname.encode("utf-8")),
                                articletype, ctypes.byref(length),
                                ctypes.c_longlong(len(fieldname.encode("utf-8"))), ctypes.c_longlong(2))
                assert returncode.value == 0, "Error reading type and length of article " + fieldname + \
                                              " on file " + self.filename
                return articletype.value.decode('utf-8'), length.value
            def _wlfalecr(self, fieldname, fieldlength):
                return self._wlfalec(fieldname, fieldlength, numpy.float64)
            def _wlfaleci(self, fieldname, fieldlength):
                return self._wlfalec(fieldname, fieldlength, numpy.int64)
            def _wlfalec(self, fieldname, fieldlength, dtype):
                returncode, length = ctypes.c_longlong(), ctypes.c_longlong()
                result = numpy.ndarray((fieldlength, ), dtype=dtype, order='F')
                f = {numpy.float64:_solib.wlfalecr_,
                     numpy.int64:_solib.wlfaleci_}[dtype]
                f.argtypes = [ctypes.POINTER(ctypes.c_longlong), #status
                              ctypes.POINTER(ctypes.c_longlong), #logical unit
                              ctypes.c_char_p, #article name
                              ctypes.POINTER(ctypes.c_longlong), #array physical size
                              numpy.ctypeslib.ndpointer(dtype=dtype,
                                                        ndim=1,
                                                        flags=str('F_CONTIGUOUS')),  # Note: str() needed in Python2 for unicode/str obscure inner incompatibility,
                              ctypes.POINTER(ctypes.c_longlong), #array size
                              ctypes.c_longlong] #article name string length
                f(ctypes.byref(returncode),
                               ctypes.byref(ctypes.c_longlong(self._unit)),
                               ctypes.create_string_buffer(fieldname.encode("utf-8")),
                               ctypes.byref(ctypes.c_longlong(fieldlength)),
                               result, ctypes.byref(length),
                               ctypes.c_longlong(len(fieldname.encode("utf-8"))))
                assert returncode.value == 0, "Error reading article " + fieldname + \
                                              " on file " + self.filename
                return result, length.value
            def _wlfalecc(self, fieldname, fieldlength):
                returncode, length = ctypes.c_longlong(), ctypes.c_longlong()
                result = numpy.ndarray((fieldlength,), dtype=('S', self.LFA_maxstrlen), order='F')
                result = numpy.asfortranarray(numpy.core.defchararray.ljust(result, self.LFA_maxstrlen))
                _solib.wlfalecc_.argtypes = [ctypes.POINTER(ctypes.c_longlong), #status
                                             ctypes.POINTER(ctypes.c_longlong), #logical unit
                                             ctypes.c_char_p, #article name
                                             ctypes.POINTER(ctypes.c_longlong), #array physical size
                                             ctypes.POINTER(ctypes.c_longlong), #string size
                                             numpy.ctypeslib.ndpointer(dtype=('S', self.LFA_maxstrlen),
                                                                       ndim=1,
                                                                       flags=str('F_CONTIGUOUS')),  # Note: str() needed in Python2 for unicode/str obscure inner incompatibility,
                                             ctypes.POINTER(ctypes.c_longlong), #array size
                                             ctypes.c_longlong] #article name string length
                _solib.wlfalecc_(ctypes.byref(returncode),
                                 ctypes.byref(ctypes.c_longlong(self._unit)),
                                 ctypes.create_string_buffer(fieldname.encode("utf-8")),
                                 ctypes.byref(ctypes.c_longlong(fieldlength)),
                                 ctypes.byref(ctypes.c_longlong(self.LFA_maxstrlen)),
                                 result, ctypes.byref(length),
                                 ctypes.c_longlong(len(fieldname.encode("utf-8"))))
                assert returncode.value == 0, "Error reading article " + fieldname + \
                                              " on file " + self.filename
                result = [result[i].strip().decode() for i in range(fieldlength)]
                return result, length.value
            def close(self):
                _solib.wlfafer_.argtypes = [ctypes.POINTER(ctypes.c_longlong)] #logical unit
                _solib.wlfafer_(ctypes.byref(ctypes.c_longlong(self._unit)))
            def __enter__(self):
                return self
            def __exit__(self, exc_type, exc_value, traceback):
                return self.close()
    return reader
