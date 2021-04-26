#!/usr/bin/env python3
import sys, os
sys.path.insert(0, '../../../pySTEL/')
import numpy as np                    #For Arrays
from math import pi
from libstell.stellopt import read_stellopt

try:
	qtCreatorPath=os.environ["STELLOPT_PATH"]
except KeyError:
	print("Please set environment variable STELLOPT_PATH")
	sys.exit(1)

failtol = 15.0
filename='stellopt.RECON_TOK'
data=read_stellopt(filename)
if not data:
    print('ERROR Opening File: '+filename)
    sys.exit(0)

print('STELLOPT_VERSION: ' + str(data['VERSION']))
print('==== Scalars ====')
varlist={}
varlist['CURTOR_equil']=1.389148172383E+006
#print(data)
lfail = 0;
for temp in varlist:
    act = varlist[temp]
    cal = data[temp]
    perct = 100*(abs(act-cal)/act)
    print('  '+temp+': '+str(cal)+'   '+str(act)+'   '+str(int(perct))+'%')
    if perct > failtol:
        lfail = 1


print('==== Vectors ====')
varlist={}

#print(repr(data['VISBREMLINE_equil']))

varlist['VISBREMLINE_equil']=np.array([ 316.75065021, 1059.8022018 ,  566.22876231])
varlist['XICS_BRIGHT_equil']=np.array([343.98692425, 344.7443,     346.97243891, 350.53233674, 355.17903401, \
 360.5546112,  366.18437659, 371.46896176, 375.67504637, 377.94230007, \
 377.31129416, 372.79147954, 363.50406125, 348.98160525, 330.14520108, \
 309.31521787, 286.74842821, 262.4816563,  236.61908959, 209.38406289, \
 181.3060784,  153.00076721, 124.7154621,   96.68114798,  68.9554765, \
  41.24595679])
varlist['XICS_equil']=np.array([2696.81734382, 2695.06423501, 2689.54506166, 2679.52262385,
       2663.92703479, 2641.59080807, 2611.53177669, 2573.23940387,
       2526.9433534 , 2473.81542771, 2416.00568346, 2356.29895878,
       2296.71513288, 2233.54744075, 2164.78636224, 2093.11590398,
       2018.94829678, 1942.17487542, 1861.5317244 , 1779.6407542 ,
       1704.69661612, 1644.61053405, 1598.29717713, 1555.56398447,
       1511.04213286, 1458.385343  ])
varlist['XICS_W3_equil']=np.array([0.23715788, 0.23776453, 0.2395927 , 0.24266425, 0.24700569,
       0.25263606, 0.25954181, 0.26763146, 0.2766502 , 0.28605022,
       0.29481939, 0.30138069, 0.30398224, 0.30270051, 0.2986845 ,
       0.29326553, 0.2865379 , 0.27839688, 0.26859545, 0.25586998,
       0.23811462, 0.21322361, 0.18082623, 0.1441176 , 0.10576884,
       0.066104  ])
varlist['XICS_V_equil']=np.array([-1.41610840e-04,  2.42276755e+02,  4.80561378e+02,  7.13741091e+02,
        9.40970075e+02,  1.16157923e+03,  1.37494125e+03,  1.57996408e+03,
        1.77490819e+03,  1.95740965e+03,  2.12445484e+03,  2.27246910e+03,
        2.39765905e+03,  2.49640538e+03,  2.56731781e+03,  2.61236826e+03,
        2.63016045e+03,  2.61864742e+03,  2.57559803e+03,  2.49854961e+03,
        2.38546768e+03,  2.23504684e+03,  2.04560484e+03,  1.81569549e+03,
        1.54154701e+03,  1.20696598e+03])
varlist['TE_equil']=np.array([   0.        ,    0.        ,    0.        ,    0.        ,
          0.        ,    0.        ,   96.36314418,  356.06094368,
        552.6692318 ,  607.44894795,  657.16899228,  702.05870588,
        750.56117135,  797.51234239,  845.96174776,  887.40064729,
        914.59331377,  930.31649267,  951.44181176,  989.28644469,
       1051.91406332, 1183.10047558, 1304.77154607, 1431.88687992,
       1397.53286054, 1490.18928231, 1589.01680918, 1830.70521164,
       2167.0050753 , 2339.26509563, 2320.73684564, 2319.85677979,
       2791.00575672, 4007.6790705 , 4107.53026837, 3897.38701877])
varlist['NE_equil']=np.array([0.24731439, 0.48781451, 0.61845877, 0.69053874, 0.72464231,
       0.73164806, 0.71158871, 0.6744063 , 0.6456336 , 0.6501305 ,
       0.67082465, 0.69418575, 0.72013749, 0.74869723, 0.79012389,
       0.86775073, 0.84868341, 0.85508997, 0.84055251, 0.87222949,
       0.89011998, 0.92121854, 0.96754153, 0.99210262, 0.98964058,
       0.98952226, 1.01319589, 1.00264154, 1.00121449, 1.00418141])
varlist['TI_equil']=np.array([3875.96899291, 3211.81824507, 2664.34040351, 2259.74997237,
       1840.15393787, 1422.1244813 , 1134.31910036, 1088.68723224,
        848.22515555, 3497.73806627, 2900.64831025, 2429.65704724,
       1874.02009776, 1604.04078095, 1241.29258548,  967.63022886,
        513.63507596])
varlist['B_PROBES_equil']=np.array([0.615338954491, 0.4967882406037, 0.5969360858376, 0.4975297983739, 0.1347921404238, 0.1275233159043, 0.1403953152371, 0.1435136716622, 0.06744195761294, 0.09196017155826, 0.1103327441662, 0.08312021962863, 0.1044396688734, 0.122055958714, 0.1349132029292, 0.1514532107701, 0.1356200558251, 0.124602600166, 0.1063195978977, 0.08643194033318, 0.0834965624624, 0.04440201675469, 0.1406887081933, 0.1415210249121, 0.129508817632, 0.2330349789787, 0.2648309928132, 0.3662429751138, 0.4994098498812, 0.5973420609288, 0.615444302477, 0.5983135021876, 0.4996271766421, 0.3679281166058, 0.2641644424955, 0.2342334275953, 0.03552753109768, 0.05529743591858, 0.2116829558592, 0.1856112465757, 0.23836072547, 0.140406610473, 0.3218470712797, 0.1659367070517, 0.1654227009001, 0.1604801076034, 0.162011928222, 0.1658930732637, 0.1670668080705, 0.1649200837048, 0.1678584389493, 0.1680015047679, 0.1533575186413, 0.1534897593437, 0.153328878263, 0.1524944789053, 0.15346882826, 0.1534428825217, 0.1533180324158, 0.1533286802881, 0.1539492816116, 0.1534123279665, 0.1034386963007, 0.1037020359958, 0.1030065459774, 0.1044505268963, 0.1031423034563, 0.1038686662316, 0.1048295891538, 0.1056319987034, 0.1057103848788, 0.1054320025962, 0.1045476364202, 0.1054141750064])
varlist['FLUXLOOPS_equil']=np.array([-1.313299859874, -1.157584855333, -0.9252157527953, -0.6907530692406, -0.6844403152354, -3.463540755212, 0.0, -0.8935939492658, -1.401699637063, -1.312184395269, -1.152712926269, -0.9186311359517, -0.6909612449817, -0.6860793526284, -3.463854957372, -2.560161991045, -0.8897556052543, -1.413725698367, -1.442607678473, -1.346438506618, -1.115086786915, -0.8518739142455, -0.7048719637096, -0.8084725537337, -1.480838102289, -2.197085465305, -2.469471430406, -3.23979142867, -3.383934698999, -1.346438522451, -1.115086801816, -0.8518738280982, -0.7041769241914, -0.8084829873405, -1.618801488541, -2.196411751211, -2.474723625928, -3.243973134086, -3.391087183729, -1.143508906011, -1.369192043319, -1.352277106605, -1.624491573311, -1.811950341798, 0.02343673020368, 0.02344662940595, 0.01174029661343, 0.001544964266941, -0.006812328705564, 0.0, -0.01956845261157, -0.02468682546169, 0.04482747102156, 0.05818241089582, 0.04892003489658, 0.0, -0.0472377021797, -0.05029140226543, -0.05408232333139, 0.02467990992591, 0.01957051083446, -0.01956823188812, -0.0246868650429, 0.04527894360945, 0.05847896914929, 0.04973966861144, 0.0002665361389518, -0.04896385967731, -0.05084964167327, -0.05430325865746, 0.02468025265262, 0.01957079349086, 0.02898896113038, 0.005754689220339, 0.01030017295852, -0.01033029705286, 0.01006310619414, -0.01007274910808, -0.004129649372771, -0.02067906968296, 5.381554365158e-05, 5.44348731637e-05, 5.447771400213e-05, 5.387794226408e-05, 0.0, 5.453359335661e-05, 1.407414674759e-05, 1.421011984348e-05, 1.334119588137e-05, 0.0, 1.422967761755e-05, 1.367274671793e-05, 0.1195037951693, 0.09473634790629, 0.1385387955233, 0.1165434131399, 0.09473635395989, 0.1355543634854, -0.1159437615424, -0.09472840558738, -0.138529933989, -0.1165355062112, -0.09472838789225, -0.1391273103654])
varlist['MSE_equil']=np.array([ 1.42384177e-17,  1.02178317e-01,  7.26283949e-02,  3.73710513e-02,
       -2.97207635e-03,  0.00000000e+00, -8.85949973e-02, -1.28181548e-01,
       -1.64008731e-01, -1.94677275e-01, -2.22523718e-01,  1.85641710e-01,
        1.98326373e-01,  2.02762748e-01,  2.01253371e-01, -0.00000000e+00,
        1.89993916e-01,  1.76233430e-01,  1.75760846e-01, -1.48657858e-18,
        6.47123445e-03, -0.00000000e+00,  8.68306824e-02,  1.17829809e-01,
        1.41486051e-01, -0.00000000e+00,  1.62121549e-01, -0.00000000e+00,
       -2.42113498e-01, -2.55746881e-01, -2.67936689e-01, -2.69212450e-01])
varlist['SEPARATRIX_equil']=np.array([-0.00709641,  0.10148895])
for temp in varlist:
    act = varlist[temp]
    cal = data[temp]
    cal = np.where(act==0,0,cal)
    div = np.where(act==0,1,act)
    perct = 100*sum(abs(act-cal)/div)
    print('  '+temp+': '+str(cal[0])+'   '+str(act[0])+'   '+str(int(perct))+'%')
    if perct > failtol:
        lfail = 1
print('=================')

if (lfail):
    print('  STATUS: FAIL!!!!!')
else:
    print('  STATUS: PASS')

sys.exit(0)




