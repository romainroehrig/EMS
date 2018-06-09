#saveall = True
saveall = False

var2save = ['pf','zf','temp','qv','u','v','ustar','t2m','cc','rneb','lhf','shf','theta','ql','qi','qr','qsn','tke','rain','tsurf']


# Pour convert2p, niveau en hPa
levout = []
for i in range(1,41):
  levout.append(i*25.)
levout.reverse()

levoutz = []
for i in range(0,401):
  levoutz.append(i*50.)
levoutz.reverse()

# Niveau de print (0, 1 ou 2)
verbose = 2
