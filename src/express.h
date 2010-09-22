/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 * 
*************************************************************/

/********************************************************************
*
*  File: express.h
*
*  Contents: defines for expression parsing and evaluation.
*/

#ifdef __cplusplus
extern "C" {
#endif

/* node types, numbered above 1512 to avoid yacc token numbers */
#define GEN_ERROR_TOKEN 10000

#define IMMEDIATE_AUTOPOP_ 2337
#define PROCEDURE_CALL_RETURN_ 2336
#define FUNCTION_CALL_RETURN_ 2335
#define SETUP_FRAME_ 2334
#define CONTENT_RANK_ 2333
#define SLICE_COEFF 2332
#define CLIP_COEFF 2331
#define AUTOPOP_QUARTIC_ 2330
#define STAR_FINAGLING_ 2329
#define FORCE_DELETION_ 2328
#define DEFINE_FIXED_LOCAL_ARRAY_ 2327
#define ARRAY_VERTEX_NORMAL_ 2326
#define ARRAY_EDGE_VECTOR_ 2325
#define ARRAY_FACET_NORMAL_ 2324
#define PRINT_ARRAY_LVALUE_ 2323
#define ARRAY_EVAL_ 2322
#define PRINT_PROFILING_ 2321
#define ARRAY_ASSIGNOP_FACET_NORMAL_ 2320
#define ARRAY_ASSIGNOP_EDGE_VECTOR_ 2319
#define ARRAY_ASSIGNOP_VERTEX_NORMAL_ 2318
#define ARRAY_ASSIGNOP_SINGLE_ 2317
#define ARRAY_LVALUE_INDEXED_ 2316
#define ATTRIB_LVALUE_ 2315
#define ARRAY_RVALUE_  2314
#define ARRAY_ASSIGNOP_ARRAY_  2312
#define ARRAY_ASSIGNOP_SCALAR_ 2311
#define ARRAY_ASSIGNOP_S_X_A_ 2310
#define ARRAY_ASSIGNOP_A_P_A_ 2309
#define ARRAY_ASSIGNOP_A_S_A_ 2308
#define V_HIGH_BOUNDARY 2307
#define V_HIGH_CONSTRAINT 2306
#define LOCAL_LIST_START_ 2305
#define CLIP_VIEW_ 2304
#define V_STRING_CURVE_TOLERANCE 2303
#define TEXT_SPOT_ 2302
#define WHEREAMI_COMMAND_   2301
#define UNSET_BREAKPOINT_ 2300
#define SET_BREAKPOINT_ 2299
#define PUSH_PARAM_FIXED 2298
#define V_CORONA_STATE 2297
#define PUSH_ELEMENT_ID_ 2296
#define ELINDEX_ 2295
#define SET_CONSTRAINT_GLOBAL 2294
#define UNSET_CONSTRAINT_GLOBAL 2293
#define SET_CONSTRAINT_NAME_GLOBAL 2292
#define UNSET_CONSTRAINT_NAME_GLOBAL 2291
#define FUNCTION_QUANTITY_SPARSE_ 2290
#define SLICE_VIEW_ 2289
#define V_MPI_MAXTASK  2288
#define DISPLAY_ORIGIN_ 2287
#define V_AUTOCHOP_LENGTH 2286
#define QUIETLOAD_     2285
#define LITTLE_ENDIAN_ 2284
#define BIG_ENDIAN_    2283
#define BINARY_PRINTFHEAD_ 2282
#define V_FACET_REVERSE_COUNT 2281
#define V_EDGE_REVERSE_COUNT 2280
#define V_THIS_TASK 2279
#define POP_ENJOIN_ 2278
#define V_WINDOW_ASPECT_RATIO 2277
#define PRINT_SINGLE_LETTER_ 2276
#define PDELTA_LVALUE_   2275
#define PSCALE_LVALUE_   2274


#define INIT_VERTEX_EDGE_  1513
#define INIT_VERTEX_FACET_  1514
#define INIT_VERTEX_BODY_ 1515
#define INIT_EDGE_VERTEX_ 1516
#define INIT_EDGE_FACET_  1517
#define INIT_EDGE_BODY_ 1518
#define INIT_FACET_VERTEX_  1519
#define INIT_FACET_EDGE_  1520
#define INIT_FACET_BODY_ 1521
#define INIT_BODY_VERTEX_  1522
#define INIT_BODY_EDGE_  1523
#define INIT_BODY_FACET_ 1524
#define NEXT_VERTEX_EDGE_  1525
#define NEXT_VERTEX_FACET_  1526
#define NEXT_VERTEX_BODY_ 1527
#define NEXT_EDGE_VERTEX_  1528
#define NEXT_EDGE_FACET_  1529
#define NEXT_EDGE_BODY_ 1530
#define NEXT_FACET_VERTEX_  1531
#define NEXT_FACET_EDGE_  1532
#define NEXT_FACET_BODY_ 1533
#define NEXT_BODY_VERTEX_  1534
#define NEXT_BODY_EDGE_  1535
#define NEXT_BODY_FACET_ 1536
#define NEXT_VERTEX_  1537
#define NEXT_EDGE_  1538
#define NEXT_FACET_  1539
#define NEXT_BODY_  1540
#define NEXT_ELEMENT_  1541
#define INIT_VERTEX_  1542
#define INIT_EDGE_  1543
#define INIT_FACET_  1544
#define INIT_BODY_  1545
#define INIT_ELEMENT_  1546
#define SET_GRAVITY_      1547
#define SET_DIFFUSION_      1548
#define SET_GLOBAL_      1549
#define SET_COLOR_      1550
#define SET_DENSITY_      1551
#define SET_PRESSURE_      1552
#define SET_VOLUME_      1553
#define SET_CONSTRAINT_      1554
#define SET_TAG_      1555
#define SET_OPACITY_      1556
#define SET_SCALE_      1557
#define SET_COORD_    1558
#define SET_COORD_1      1559
#define SET_COORD_2      1560
#define SET_COORD_3      1561
#define SET_COORD_4      1562
#define SET_COORD_5      1563
#define SET_COORD_6      1564
#define SET_COORD_7      1565
#define SET_COORD_8      1566
#define SET_PROCEDURE_      1567
#define SET_PROC_END_      1568
#define SET_AUTOCHOP_      1570
#define SET_GAP_CONSTANT_      1571
#define SET_AMBIENT_PRESSURE_      1572
#define SET_FIXED_AREA_      1573
#define SET_COLORMAP_      1574
#define SET_THICKEN_      1575
#define SET_BACKGROUND_      1576
#define SET_OPTIMIZE_      1577
#define SET_FIXED_ 1578
#define GET_FIXED_ 1579
#define GET_LENGTH_ 1580
#define GET_VALENCE_ 1581
#define GET_AREA_ 1582
#define GET_VOLUME_ 1583
#define GET_DENSITY_ 1584
#define GET_ID_ 1585
#define GET_TAG_ 1586
#define GET_ORIGINAL_ 1587
#define INIT_AVG_ 1588
#define INIT_SUM_ 1589
#define INIT_MAX_ 1590
#define INIT_MIN_ 1591
#define INIT_COUNT_ 1592
#define AGGREGATE_INIT_ 1593
#define AGGREGATE_END_ 1594
#define AGGREGATE_ 1595
#define SET_INIT_ 1596
#define UNSET_CONSTRAINT_ 1597
#define UNSET_BOUNDARY_ 1598
#define GET_COLOR_ 1599
#define GET_SQ_MEAN_CURV_ 1600
#define GET_INTERNAL_ 1601
#define V_VERTEXCOUNT 1602
#define V_EDGECOUNT 1603
#define V_FACETCOUNT 1604
#define V_BODYCOUNT 1605
#define V_FACETEDGECOUNT 1606
#define V_ENERGY 1607
#define V_AREA 1608
#define V_LENGTH 1609
#define V_SCALE 1610
#define LIST_PROCS_ 1611
#define PRINTFHEAD_ 1612
#define PREPRINTF_  1613
#define EXPRLIST_  1614
#define SPRINTFHEAD_ 1615
#define SET_SGLOBAL_      1616
#define GET_OID_ 1617
#define GET_FRONTCOLOR_ 1618
#define GET_BACKCOLOR_  1619
#define SET_FRONTCOLOR_ 1620
#define SET_BACKCOLOR_  1621
#define SET_PARAM_      1622
#define SET_PARAM_1      1623
#define SET_PARAM_2      1624
#define SET_PARAM_3      1625
#define SET_PARAM_4      1626
#define SET_PARAM_5      1627
#define SET_PARAM_6      1628
#define SET_PARAM_7      1629
#define SET_ATTRIBUTE_    1630
#define SET_PHASE_    1631
#define GET_PHASE_    1632
#define LEXERROR      1633
#define PRESPRINTF_      1634
#define V_SURFACE_DIMENSION 1635
#define V_SPACE_DIMENSION 1636
#define TOGGLEVALUE      1637
#define V_TORUS         1638
#define V_TORUS_FILLED    1639
#define V_SYMMETRY_GROUP 1640
#define V_SIMPLEX      1641
#define V_INTEGRAL_ORDER 1642
#define GET_STAR_        1643
#define GET_PRESSURE_      1644
#define SET_INTERNAL_      1645
#define V_TOLERANCE     1646
#define AUTODISPLAY_    1647
#define V_EQUI_COUNT    1648
#define V_DELETE_COUNT    1650
#define V_REFINE_COUNT    1651
#define V_NOTCH_COUNT    1652
#define V_DISSOLVE_COUNT    1653
#define V_POP_COUNT    1654
#define V_WHERE_COUNT    1655
#define PUSH_NAMED_QUANTITY 1656
#define SET_NAMED_QUANTITY_ 1657
#define GET_USERATTR_ 1658
#define GET_QUANTITY_ 1659
#define UNSET_NAMED_QUANTITY_ 1660
#define GET_WRAP_ 1661
#define FINISHED    1701
#define PUSHCONST  1702
#define PUSHPARAM  1703
#define PUSHPI      1704
#define PUSHE    1705
#define PLUS     1706
#define MINUS    1707
#define TIMES    1708
#define DIVIDE      1709
#define INTPOW     1710
#define POW     1711
#define SIN     1712
#define COS     1713
#define TAN     1714
#define SQRT    1715
#define LOG     1716
#define EXP     1717
#define COPY    1718
#define ACOS    1719
#define ASIN    1720
#define ATAN    1721
#define CHS     1722
#define INV     1723
#define SQR     1724
#define PUSHG      1725
#define EQUATE     1726
#define PUSHADJUSTABLE 1727
#define ABS     1728
#define USERFUNC  1729
#define REPEAT_    1730
#define FULLEXPR  1731
#define SINH  1732
#define COSH  1733
#define REPLACECONST 1734
#define REALMOD     1735
#define CEIL_     1736
#define FLOOR_     1737
#define ATAN2_     1738
#define NOP_    1739
#define V_HESS_EPSILON     1740
#define HESSIAN_DIFF_ 1741
#define SHOW_INNER_    1742
#define SHOW_OUTER_    1743
#define CLIPPED_CELLS_ 1744
#define RAW_CELLS_    1745
#define CONNECTED_CELLS_ 1746
#define NORMAL_MOTION_  1747
#define RUNGE_KUTTA_  1748
#define DETURCK_    1749
#define KUSNER_    1750
#define VIEW_4D_    1751
#define CONF_EDGE_SQCURV_ 1752
#define SQGAUSS_  1753
#define AUTOPOP_ 1754
#define OLD_AREA_ 1755
#define APPROX_CURV_ 1756
#define CHECK_INCREASE_ 1757
#define DEBUG_ 1758
#define MEMDEBUG_ 1759
#define EFFECTIVE_AREA_ 1760
#define ESTIMATE_  1761
#define POST_PROJECT_ 1762
#define TRANSFORMS_ 1763
#define QUIET_ 1764
#define CONJ_GRAD_ 1765
#define HOMOTHETY_ 1766
#define FACET_COLORS_ 1767
#define SHADING_  1768
#define DIV_NORMAL_CURVATURE_ 1769
#define NORMAL_CURVATURE_ 1770
#define BOUNDARY_CURVATURE_ 1771
#define SELF_SIMILAR_ 1772
#define GV_BINARY_ 1773
#define METRIC_CONVERSION_ 1774
#define AUTORECALC_ 1775
#define PINNING_ 1776
#define FORCE_POS_DEF_ 1777
#define V_SCALE_SCALE 1778
#define GET_EXTRA_ATTR_ 1779
#define SET_EXTRA_ATTR_ 1780
#define SET_ATTRIBUTE_LOOP_  1781
#define SET_ATTRIBUTE_L    1782
#define TANH    1783
#define ATANH    1784
#define ASINH    1785
#define ACOSH    1786
#define V_ITER_COUNTER 1787
#define QUIETGO_ 1788
#define GET_MIDV_ 1789
#define RIBIERE_CG_ 1790
#define ASSUME_ORIENTED_ 1791
#define HESSIAN_QUIET_ 1792
#define CONJUNCTION_END 1793
#define JIGGLE_TOGGLE_ 1794
#define V_TIME 1795
#define V_JIG_TEMP 1796
#define SYMBOL_ELEMENT_ 1797
#define SINGLE_ELEMENT_ 1798
#define INDEXED_SUBTYPE_ 1799
#define INDEXED_ATTRIBUTE 1800
#define QUALIFIED_ATTRIBUTE 1801
#define STRPRINT_ 1802
#define GET_TRIPLE_PT_ 1804
#define SET_TRIPLE_PT_ 1805
#define GET_TETRA_PT_ 1806
#define SET_TETRA_PT_ 1807
#define UNSET_TETRA_PT_ 1808
#define UNSET_TRIPLE_PT_ 1809
#define PUSHQPRESSURE_ 1810
#define PUSHQTARGET_ 1811
#define PUSHQVALUE_ 1812
#define PUSHQMODULUS_ 1813
#define SET_QMODULUS_ 1814
#define SET_QTARGET_ 1815
#define YSMP_      1816
#define BUNCH_KAUFMAN_ 1817
#define V_EIGENPOS 1818
#define V_EIGENNEG 1819
#define V_EIGENZERO 1820
#define QUANTITIES_ONLY_ 1821
#define EVERYTHING_QUANTITIES_ 1822
#define METRIC_CONVERT_ 1823
#define GET_TARGET_ 1824
#define MAXIMUM_ 1825
#define UNSET_FACET_BODY_ 1826
#define SET_ORIENTATION_ 1827
#define V_PICKVNUM 1828
#define V_PICKENUM 1829
#define V_PICKFNUM 1830
#define LINEAR_METRIC_ 1831
#define V_LINEAR_METRIC_MIX 1832
#define INVOKE_P_MENU_ 1833
#define GEOMVIEW_TOGGLE_ 1834
#define DO_TOP_ 1835
#define DO_END_ 1836
#define DO_ENTRY_ 1837
#define SET_MODEL_ 1838
#define V_RANDOM_SEED 1839
#define MINIMUM_ 1840
#define V_INTEGRAL_ORDER_1D 1841
#define V_INTEGRAL_ORDER_2D 1842
#define GET_ORIENTATION_ 1843
#define SET_TARGET_ 1844
#define UNSET_FIXED_  1845
#define UNSET_DENSITY_  1846 
#define UNSET_VOLUME_    1847
#define UNSET_PRESSURE_  1848
#define UNSET_TARGET_  1849
#define GET_VOLCONST_  1850
#define SET_VOLCONST_  1851
#define GET_TORUS_PERIODS_  1852
#define PUSHQVOLCONST_  1853
#define GET_FIXEDVOL_  1854
#define SET_QVOLCONST_  1855
#define SET_QPARAMETER_1_  1856
#define PUSHQPARAMETER_1_  1857
#define SINGLE_ELEMENT_INIT_  1858
#define REDEFINE_SINGLE_  1859
#define UNREDEFINE_SINGLE_  1860
#define V_QUADRATIC_METRIC_MIX 1861
#define GEOMPIPE_TOGGLE_ 1862
#define V_LAST_EIGENVALUE 1863
#define V_LAST_HESSIAN_SCALE 1864
#define SQUARED_GRADIENT_ 1865
#define PRINT_LETTER_ 1866
#define REDIRECT_END_ 1867
#define PIPE_END_ 1868
#define V_LAGRANGE_ORDER 1869
#define SET_AXIAL_POINT_ 1870
#define UNSET_AXIAL_POINT_ 1871
#define GET_AXIAL_POINT_ 1872
#define H_INVERSE_METRIC_ 1873
#define HELP_KEYWORD 1874
#define SKINNY_ 1875
#define TORDUP_ 1876
#define V_GAP_CONSTANT 1877
#define V_THICKNESS 1878
#define V_TARGET_TOLERANCE 1879
#define V_CLOCK 1880
#define FIX_QUANTITY_ 1881
#define UNFIX_QUANTITY_ 1882
#define SET_ORIGINAL_ 1883
#define V_SCALE_LIMIT 1884
#define SET_MMODULUS_ 1885
#define GET_INSTANCE_ 1886
#define PUSHMMODULUS_ 1887
#define PUSHMVALUE_    1888
#define PSCOLORFLAG_  1889
#define GRIDFLAG_      1890
#define CROSSINGFLAG_ 1891
#define LABELFLAG_     1892
#define SHOW_ALL_QUANTITIES_ 1893
#define GET_INVERSE_PERIODS_  1894
#define CREATE_EDGE_  1895
#define SET_NO_REFINE_ 1896
#define UNSET_NO_REFINE_ 1897
#define GET_NO_REFINE_ 1898
#define CREATE_VERTEX_  1899
#define CREATE_FACET_  1900
#define CREATE_BODY_  1901
#define SET_FRONTBODY_ 1902
#define SET_BACKBODY_ 1903
#define V_TRANSFORM_COUNT 1904
#define GT_        1905
#define LT_        1906
#define GET_BACKBODY_  1907
#define GET_FRONTBODY_ 1908
#define GET_TRANSFORM_EXPR_ 1909
#define LOGFILE_TOGGLE_ 1910
#define SET_WRAP_ 1911
#define PLUSASSIGN_ 1912
#define SUBASSIGN_ 1913
#define MULTASSIGN_ 1914
#define DIVASSIGN_ 1915
#define NULLBLOCK_ 1916
#define SINGLE_ASSIGN_ 1917
#define SET_ATTRIBUTE_A 1918
#define SET_METHOD_INSTANCE_ 1919
#define UNSET_METHOD_INSTANCE_ 1920
#define PUSH_METHOD_INSTANCE_ 1921
#define LIST_ATTRIBUTES_ 1922
#define NULLCMD_ 1923
#define FIX_PARAMETER_ 1924
#define UNFIX_PARAMETER_ 1925
#define ITDEBUG_ 1926
#define ZENER_DRAG_ 1927
#define VOLGRADS_EVERY_ 1928
#define V_RANDOM 1929
#define PUSHQTOLERANCE_ 1930
#define SET_QTOLERANCE_ 1931
#define PUSHDELTA_  1932
#define SET_DELTA_  1933
#define BACKCULL_  1934
#define V_BRIGHTNESS 1935
#define V_DIFFUSION 1936
#define DEFINE_IDENT_ 1937
#define V_BACKGROUND 1938
#define SET_NO_DISPLAY_ 1939
#define UNSET_NO_DISPLAY_ 1940
#define GET_NO_DISPLAY_ 1941
#define V_MEMARENA 1942
#define V_MEMUSED 1943
#define INDEXED_COORD_ 1944
#define V_LAST_ERROR 1945
#define VERBOSE_ 1946
#define ACOMMANDEXPR_ 1947
#define CMDLIST_ 1948
#define ULONG_TYPE_ 1949
#define INDEXED_ELEMENT_ 1950
#define MEAN_CURV_ 1951
#define REPEAT_INIT_ 1952
#define SYMATTR_ 1953
#define INIT_SUBELEMENT_ 1954
#define GET_DIHEDRAL_ 1955
#define INIT_FACETEDGE_ 1956
#define GET_EDGE_ 1957
#define GET_FACET_ 1958
#define NEXT_FACETEDGE_ 1959
#define PUSHGLOBAL_ 1960
#define COMMAND_BLOCK_ 1961
#define IFTEST_ 1962
#define WHILE_TOP_ 1963
#define WHILE_END_ 1964
#define PRINT_PROCEDURE_ 1965
#define SHOW_END_ 1966
#define AMBIENT_PRESSURE_ 1967
#define INTERP_NORMALS_ 1969
#define MEAN_CURV_INT_ 1970
#define COND_TEST_ 1971
#define COND_EXPR_ 1972
#define COND_ELSE_ 1973
#define BACKGROUND_ 1974
#define V_AMBIENT_PRESSURE 1975
#define V_HESSIAN_SLANT_CUTOFF 1976
#define WRAP_COMPOSE_ 1977
#define WRAP_INVERSE_ 1978
#define GET_VERTEXNORMAL_ 1979
#define SET_VIEW_MATRIX_ 1980
#define VIEW_MATRIX_LVALUE_ 1981
#define ACTUAL_VOLUME_ 1982
#define SELF_ELEMENT_  1983
#define UNSET_FRONTBODY_ 1984
#define UNSET_BACKBODY_ 1985
#define V_CHECK_COUNT_ 1986
#define V_BREAKFLAG_ 1987
#define SET_CONSTRAINT_NAME 1988
#define UNSET_CONSTRAINT_NAME 1989
#define SET_BOUNDARY_NAME 1990
#define UNSET_BOUNDARY_NAME 1991
#define ON_CONSTRAINT_NAME 1992
#define HIT_CONSTRAINT_NAME 1993
#define ON_BOUNDARY_NAME 1994
#define SET_PARAM_SCALE 1995
#define PUSH_PARAM_SCALE 1996
#define EXPRINT_PROCEDURE_ 1997
#define ERRPRINTFHEAD_ 1998
#define INDEXSET_ 1999
#define DEFINE_ARRAY_ 2000
#define ARRAYEVAL 2001
#define ARRAYASSIGN 2002
#define ARRAY_HEAD_ 2003
#define VIEW_TRANSFORMS_NOP_ 2004
#define VIEW_TRANSFORMS_ELEMENT_ 2005
#define GET_SHOW_ 2006
#define ATTR_FUNCTION_ 2007
#define ATTR_FUNCTION_END_ 2008
#define SET_Q_FIXED_ 2009
#define SET_Q_ENERGY_ 2010
#define SET_Q_INFO_ 2011
#define SET_Q_CONSERVED_ 2012
#define KEYLOGFILE_TOGGLE_ 2013
#define ELLIPTICE 2014
#define ELLIPTICK 2015
#define INCOMPLETE_ELLIPTICF 2016
#define INCOMPLETE_ELLIPTICE 2017
#define V_VISIBILITY_DEBUG_  2018
#define V_SCROLLBUFFERSIZE_  2019
#define PUSH_PARAM_EXTRA_ 2020
#define PRINT_ARRAY_       2021
#define SET_BARE_  2022
#define GET_BARE_ 2023
#define UNSET_BARE_ 2024
#define V_PS_STRINGWIDTH_ 2025
#define V_PS_FIXEDEDGEWIDTH_ 2026
#define V_PS_TRIPLEEDGEWIDTH_ 2027
#define V_PS_CONEDGEWIDTH_ 2028
#define V_PS_BAREEDGEWIDTH_ 2029
#define V_PS_GRIDEDGEWIDTH_ 2030
#define SET_NONCONTENT_  2031 
#define UNSET_NONCONTENT_ 2032
#define GET_NONCONTENT_ 2033
#define BACKQUOTE_START_ 2034
#define BACKQUOTE_END_   2035
#define DEFINE_EXTRA_INDEX_  2036
#define FOR_END_ 2037
#define FOR_HEAD_ 2038
#define FOR_TOP_  2039
#define FOR_ENTRY_ 2040
#define SET_PERM_GLOBAL_   2041
#define SET_PERM_SGLOBAL_    2042 
#define PUSH_PERM_GLOBAL_  2043
#define SET_PERM_PROCEDURE_ 2044
#define PUSH_PERM_GLOBAL  2045
#define PRINT_PERM_PROCEDURE_ 2046
#define SET_PERM_PROC_END_ 2047
#define DIMENSIONSET_ 2048
#define PRINT_ARRAYPART_ 2049
#define DECLARE_LOCAL_ 2050
#define FUNCTION_HEAD_ 2051
#define ARGLIST_ 2052
#define SET_FUNCTION_ 2053
#define SET_FUNC_END_ 2054
#define FUNCTION_START_ 2055
#define FUNCTION_DEF_START_ 2056
#define FUNCTION_EXIT_ 2057
#define FUNCTION_CALL_ 2058
#define FUNCTION_PROTO_START_ 2059
#define FUNCTION_PROTO_ 2060
#define SET_ARGSPROC_ 2061
#define SET_ARGSPROC_END_ 2062
#define PROCEDURE_START_ 2063
#define PROCEDURE_DEF_START_ 2064
#define PROCEDURE_EXIT_ 2065
#define PROCEDURE_CALL_ 2066
#define PROCEDURE_PROTO_START_ 2067
#define PROCEDURE_PROTO_ 2068
#define PROCEDURE_HEAD_ 2069
#define PRINT_ATTR_ARRAY_ 2070
#define PRINT_VERTEXNORMAL_ 2071
#define SIZEOF_ATTR_ 2072
#define SIZEOF_ARRAY_ 2073
#define SIZEOF_STRING_ 2074
#define BEZIER_BASIS_ 2075
#define GET_MEANCURV_ 2076
#define DEFINE_EXTRA_ 2077
#define SET_BOUNDARY_ 2078
#define GET_HIT_PARTNER_ 2079
#define SET_HIT_PARTNER_ 2080
#define UNSET_HIT_PARTNER_ 2081
#define DEFINE_QUANTITY_ 2082
#define DEFINE_METHOD_INSTANCE_ 2083
#define PUSHQFIXED_  2084
#define PUSHQENERGY_  2085
#define PUSHQINFO_ONLY_  2086
#define PUSHQCONSERVED_  2087
#define SMOOTH_GRAPH_  2088
#define MPI_DEBUG_  2089
#define GET_MPI_TASK_  2090
#define POP_DISJOIN_  2091
#define DEFINE_CONSTRAINT_ 2092
#define DEFINE_BOUNDARY_ 2093
#define V_DIFFUSION_  2094
#define V_VERTEX_DISSOLVE_COUNT 2095
#define V_EDGE_DISSOLVE_COUNT 2096
#define V_FACET_DISSOLVE_COUNT 2097
#define V_BODY_DISSOLVE_COUNT 2098
#define V_EDGE_REFINE_COUNT 2099
#define V_FACET_REFINE_COUNT 2100
#define V_VERTEX_POP_COUNT  2101
#define V_EDGE_POP_COUNT 2102
#define V_POP_TRI_TO_EDGE_COUNT 2104
#define V_POP_EDGE_TO_TRI_COUNT 2105
#define V_POP_QUAD_TO_QUAD_COUNT 2106
#define V_EDGESWAP_COUNT 2107
#define V_T1_EDGESWAP_COUNT 2108
#define V_SKINNY_REFINE_COUNT 2109
#define V_VERTEX_DELETE_COUNT 2110
#define V_EDGE_DELETE_COUNT 2111
#define V_FACET_DELETE_COUNT 2112
#define V_BODY_DELETE_COUNT 2113
#define V_FIX_COUNT 2114
#define V_UNFIX_COUNT 2115
#define V_PS_LABELSIZE_ 2116
#define V_CPU_COUNTER 2117
#define GET_MID_EDGE_ 2118
#define GET_MID_FACET_ 2119
#define INIT_FACETEDGE_EDGE_ 2120
#define INIT_FACETEDGE_FACET_ 2121
#define NEXT_FACETEDGE_EDGE_ 2122
#define NEXT_FACETEDGE_FACET_ 2123

/* stuff transferred over from lex.h */
#define  NO_TOKEN        0
#define  WULFF_        2257
#define  PERIODS_     2256
#define  TORUS_        2252
#define  TORUS_FILLED_  2251
#define  SOAPFILM_    2249
#define  MOBILITY_    2248
#define  MOBILITY_TENSOR_  2247
#define  ENVECT_      2245
#define  CONVECT_     2244
#define  MERITFACTOR_    2243
#define  GRAV_CONST_     2242
#define  SPRING_CONSTANT_ 2241
#define  TEMPERATURE_      2239
#define  FACES_          2237
#define  CONVEX_         2230
#define  NONNEGATIVE_      2229
#define  NONPOSITIVE_      2228
#define  PARAMETERS_    2227
#define  CONTENT_        2222
#define  BCOORD_         2219
#define  SURFACE_ENERGY_  2216
#define  SYMMETRIC_CONTENT_ 2215
#define  SCALE_LIMIT_      2214
#define  CONSTRAINT_TOLERANCE_ 2211
#define  ZOOM_VERTEX_      2210
#define  ZOOM_RADIUS_      2209
#define  QVECT_          2206
#define  SPACE_DIMENSION_ 2205
#define  SURFACE_DIMENSION_ 2204
#define  SIMPLEX_REP_      2203
#define  METRIC_         2202
#define  SYMMETRY_GROUP_  2201
#define  UNKNOWN         2199
#define  CONFORMAL_     2198
#define  SQUARE_CURVATURE_ 2197
#define  PARAMETER_FILE_    2196
#define  TOTAL_TIME_     2191
#define  PHASEFILE_      2190
#define  KLEIN_METRIC_      2187
#define  GLOBAL_METHOD_     2185
#define  EFIXED_        2184
#define  VIEW_TRANSFORM_GENS_ 2182
#define  GAUSS_CURVATURE_     2181
#define  INSULATING_KNOT_ENERGY_ 2179
#define  CONDUCTING_KNOT_ENERGY_ 2178
#define  METHOD_     2177
#define  NONWALL_        2176
#define  SCALAR_INTEGRAND_    2175
#define  VECTOR_INTEGRAND_    2174
#define  FORM_INTEGRAND_    2173
#define  PARAMETER_1_    2172
#define  OPTIMIZING_PARAMETER_ 2171
#define  K_VEC_ORDER_ 2170
#define  LAGRANGE_ORDER_ 2169
#define  HESSIAN_DOUBLE_NORMAL_ 2168
#define  INTERP_BDRY_PARAM_ 2167
#define  HESSIAN_NORMAL_ 2166
#define  HESSIAN_NORMAL_ONE_ 2165
#define  HESSIAN_NORMAL_PERP_ 2164
#define  HESSIAN_SPECIAL_NORMAL_ 2163
#define  LOAD_LIBRARY_ 2162
#define  IGNORE_CONSTRAINTS_ 2161
#define  VERSION_ 2160
#define  KEEP_MACROS_ 2159
#define  LAGRANGE_MULTIPLIER_ 2158
#define  SWAP_COLORS_ 2157
#define  STRING_TYPE_ 2156
#define  IGNORE_FIXED_ 2155
#define  KEEP_ORIGINALS_ 2154
#define  ELEMENT_MODULUS_ 2153
#define  VOLUME_METHOD_NAME_ 2152
#define  DIRICHLET_MODE_ 2151
#define  SOBOLEV_MODE_ 2150
#define  KRAYNIKPOPVERTEX_FLAG_ 2148
#define  KRAYNIKPOPEDGE_FLAG_ 2147
#define  VERSIONTOKEN_ 2146
#define  HESSIAN_SPECIAL_NORMAL_VECTOR_ 2145
#define  RGB_COLORS_FLAG_ 2144
#define  CIRCULAR_ARC_DRAW_ 2143
#define  VISIBILITY_TEST_ 2142
#define  SPARSE_CONSTRAINTS_ 2141
#define  BLAS_FLAG_ 2140
#define  AUGMENTED_HESSIAN_ 2139
#define  AREA_METHOD_NAME_ 2138
#define  LENGTH_METHOD_NAME_ 2137
#define  BREAK_AFTER_WARNING_ 2136
#define  PARTNER_HITTING_    2135
#define  DISPLAY_PERIODS_     2132
/* end stuff transferred over from lex.h */

#define FULL_BOUNDING_BOX_ 2258
#define LIST_CONSTRAINT_   2259
#define LIST_BOUNDARY_     2260
#define LIST_QUANTITY_     2261
#define LIST_METHOD_INSTANCE_   2262
#define INIT_EDGE_FACETEDGE_ 2263
#define NEXT_EDGE_FACETEDGE_ 2264
#define POP_TO_FACE_         2265
#define POP_TO_EDGE_         2266
#define LINE_CONTINUATION    2267
#define V_MINDEG_DEBUG_LEVEL 2268
#define V_MINDEG_MARGIN      2269
#define V_MINDEG_MIN_REGION_SIZE      2270
#define SET_ELEMENT_GLOBAL_  2271
#define SINGLE_ELEMENT_EXPR_ 2272
#define UNPUTTED_            2273


/* for BREAK and CONTINUE */
extern int loopdepth;

/* tree node for expression trees */
struct treenode 
{
    int type;    /* type of node                     */
    int left;    /* left subexpression index offset  */
    int right;   /* right subexpression index offset */
    int line_no; /* line number of source file       */
    int file_no; /* number of source file            */
    int datatype; /* type of expression value        */
    int flags;
    union { int intval;            /* misc. integer data */
            int skipsize;          /* nodes to skip over */
            int indexcount;        /* number of indices  */
            int argcount;          /* number of function arguments */
            int assigntype;        /* for assignments    */
            int aggrtype;          /* aggregate type     */
            int eltype;            /* element type       */
            int maxsteps;          /* for burchard()     */
            int coordnum;
            REAL  real;            /* constant value     */
            struct sym *symptr;    /* symbol table ptrs  */
            int localnum;          /* where element id stored*/
            int extranum;          /* number of extra attr */
            element_id id;         /* element id         */
            int name_id;           /* name identifier    */
            int letter;            /* for redefine       */
            int quant_id;          /* named quantity id  */
            int meth_id;           /* named method id    */
            int con_id;            /* number of constraint */
            int bdry_id;           /* number of boundary */
            int toggle_id;
            int toggle_state;      /* ON_ or OFF_        */ 
            int bool_val;          /* 0 or 1             */
            int intpow;            /* integer power      */
            int wherecount;
            int userfunc;
            char *string;          /* string value       */
            struct expnode enode;  /* for expression     */
            dll_func_type funcptr; /* DLL function       */
          } op1;    /* operand 1*/
    union { int intval;            /* misc. integer data */
            int eltype;            /* element type       */
            int valtype;           /* data type          */
            int breakdepth;        /* number of loop to break out of */
            struct sym *symptr;    /* symbol table ptrs  */
            char *string;
            int localnum;          /* where loop element id stored*/
            int coordnum;
            int attr_kind;
            int extranum;          /* number of extra attr */
            int jumpsize;
            int assigntype;
            int argcount;
            int indexcount;
            int name_id;
            int quant_id;
            int meth_id;
          } op2;    /* operand  2 */
    union { int intval[2]; /* misc. integer data */
            int breakjump;
            int argcount;
            int argtype;
            int extra_info;       /* extra attr eltype and number */
            int extranum;
            int connum;           /* constraint number */
            int bdrynum;
            int name_id;
            int localnum;          /* where element id stored*/
           } op3;
    union { int contjump;
            int argtype;
            int extranum;
            int ret_type;
            int eltype;            /* element type       */
            int name_id;
           } op4;
    union { struct sym *symptr;   /* symbol table ptrs  */
            char *string;
            struct locallist_t *locals; /* for procedures */
          } op5;
    int stack_delta;  /* what node does to runtime stack */
#ifdef _DEBUG
    int stack_spot;  /* where stack should be after evaluation
                                  of this node. */
#endif
    int stackpos; /* local variable on stack for storing stack
                     position for chopping stack after break or continue */
};
/* flags, also used for expnode */
#define LOCAL_VAR_REF 4
#define LOCAL_VAR_REF_2 8
#define LOCAL_VAR_REF_3 0x10
#define HAS_STRING    0x20
#define EPHEMERAL     0x40  /* refers to a nonpermanent name */
#define PERMNODE      0x80  /* part of permanent command */
#define HAS_LOCALLIST    0x100
#define HAS_STRING_5  0x200
#define DEALLOCATE_POINTER 0x400
#define IN_ELEMENT_LOOP  0x800
#define BREAKPOINT_NODE  0x1000
#define IS_RVALUE        0x2000
#define SET_ASSIGNOP     0x4000
#define RECALC_NODE      0x8000
#define IS_VIRTUAL_ATTR  0x10000

/* for some bit packing */
#define ESHIFT 12

/* for some type and number packing */
#define YYTYPESHIFT 25
#define YYSHIFTMASK (((1<<YYTYPESHIFT)-1))

struct eval_frame {  /* for access to parent eval's */
   struct treenode **basenode;  /* for permload kludge */
   struct expnode *base_ex;     /* for permload kludge and debugging */
   element_id self_id;          /* reference element, if any */
   int  parent_frame_spot;             /* top-most frame location */
   struct treenode *return_node; /* for procedure return address */
   int  flags; 
};

/* flag bits */
/* #define IN_ELEMENT_LOOP  0x0800  defined elsewhere in this file */
/*    set in frame flags only if frame is in elemernt loop on entry */

#define BASE_OF_EVAL     2
#define BASE_OF_WHOLE_STACK 4
/* Magic number to act as sentinel at end of stack */
#define STACKMAGIC 12738546.2341341

#ifdef __cplusplus
}
#endif
