
def readarp(fin):
  """
    Read atmospheric namelist in fin and return a dictionary
  """

  f = open(fin)
  lines = f.readlines()
  f.close()

  namelist = {}

  for line in lines:
    tmp = line.strip()
    if tmp[0] == '&':	    
      namelist[tmp[1:]] = {}
      nam_tmp = tmp[1:]
    elif tmp[0] == '/':
      pass
    elif tmp[0].strip() == '':
      pass	     
    else:
      toto = line.split('=')
      if len(toto) == 2:
        tmp = list(toto[1].strip().split(','))
        tmp1 = [x.strip() for x in tmp if x is not('')]
        param = toto[0].strip()
        namelist[nam_tmp][param] = tmp1 #toto[1].split(',')[0].strip()
      elif len(toto) == 1:
        tmp = toto[0].strip().split(',')[0]
        namelist[nam_tmp][param].append(tmp)
      else:
        print 'case unexpected:', line

  return namelist

def writearp(nam,fout):
  """
   Write atmospheric namelist in file fout
  """

  f = open(fout,'w')

  for nn in sorted(nam.keys()):
    print >>f, '&' + nn
    for param in sorted(nam[nn].keys()):
      tmp = '  ' + param + '='	    
      for val in nam[nn][param]:
        tmp = tmp + val + ','
      print >>f, tmp	  
    print >>f, '/'


  f.close()

def readsurfex(fin):
  """
    Read namelist SURFEX in fin and return a dictionary
  """

  f = open(fin)
  lines = f.readlines()
  f.close()

  namelist = {}

  for line in lines:
    tmp0 = line.strip().split('=')
    #print tmp0
    tmp = line.strip()
    #print tmp
    if len(tmp0) == 1:
      if tmp[0] == '&':	    
        namelist[tmp[1:]] = {}
        nam_tmp = tmp[1:]
      elif tmp[0] == '/':
        pass
      elif tmp[0].strip() == '':
        pass	     
      else:
        toto = line.split('=')
        if len(toto) == 2:
          tmp = list(toto[1].strip().split(','))
          tmp1 = [x.strip() for x in tmp if x is not('')]
          param = toto[0].strip()
          namelist[nam_tmp][param] = tmp1 #toto[1].split(',')[0].strip()
        elif len(toto) == 1:
          tmp = toto[0].strip().split(',')[0]
          namelist[nam_tmp][param].append(tmp)
        else:
          print 'case unexpected 1:', line
    else:
      #print tmp0[0]
      tmp1 = line.strip().split()
      if tmp1[0][0] == '&':
        namelist[tmp1[0][1:]] = {}
        nam_tmp = tmp1[0][1:]
        n = len(nam_tmp)
        #print nam_tmp,n,line[n+1:]
        toto = line[n+1:].split('=')
        if len(toto) == 2:
          tmp = list(toto[1].strip().split(','))
          tmp1 = [x.strip() for x in tmp if x is not('')]
          param = toto[0].strip()
          namelist[nam_tmp][param] = tmp1
        elif len(toto) == 1:
          tmp = toto[0].strip().split(',')[0]
          namelist[nam_tmp][param].append(tmp)
        else:
          print 'case unexpected 2:', line
      else:
        toto = line.split('=')
        if len(toto) == 2:
          tmp = list(toto[1].strip().split(','))
          tmp1 = [x.strip() for x in tmp if x is not('')]
          param = toto[0].strip()
          namelist[nam_tmp][param] = tmp1 #toto[1].split(',')[0].strip()
        elif len(toto) == 1:
          tmp = toto[0].strip().split(',')[0]
          namelist[nam_tmp][param].append(tmp)
        else:
          print 'case unexpected 3:', line          

  return namelist


def writesurfex(nam,fout):
  """
   Write namelist SURFEX in file fout
  """

  f = open(fout,'w')

  for nn in sorted(nam.keys()):
    print >>f, '&' + nn
    for param in sorted(nam[nn].keys()):
      if len(param) <= 16:	    
        tmp = '  ' + param + ' '*(16-len(param)) +'= '	    
      else:
        tmp = '  ' + param + ' = '
      for val in nam[nn][param]:
#        print nn, param, val
        tmp = tmp + val + ','
      print >>f, tmp	  
    print >>f, '/'

  f.close()
