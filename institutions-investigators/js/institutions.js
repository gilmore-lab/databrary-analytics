var markers = [["New York University (NYU)",40.7295,-73.9965],["Pennsylvania State University, University Park",40.8148,-77.8653],["University of Wisconsin, Madison",43.0766,-89.4125],["University of Michigan",42.278,-83.7382],["University of Rochester",43.1306,-77.626],["Georgia State University",33.7531,-84.3853],["University of Southern California",34.0224,-118.2851],["Carnegie Mellon University (CMU)",40.4427,-79.943],["Rutgers University, Newark",40.7411,-74.1733],["Rutgers University, New Brunswick",40.5008,-74.4474],["University of Miami",25.7904,-80.212],["University of Virginia",38.0336,-78.508],["Indiana University",39.1746,-86.5129],["College of William & Mary",37.2717,-76.7134],["University of Texas at Austin",30.2849,-97.7341],["McMaster University",43.2609,-79.9192],["University of Chicago",41.7886,-87.5987],["Brown University",41.8268,-71.4025],["Tulane University",29.9403,-90.1207],["University of Massachusetts, Boston",42.3145,-71.0373],["Virginia Tech",37.2284,-80.4234],["University of Georgia",33.948,-83.3773],["Vanderbilt University",36.1447,-86.8027],["Texas A&M University",30.6185,-96.3365],["Yale University",41.3163,-72.9223],["Brigham Young University",40.2518,-111.6493],["Georgetown University",38.9076,-77.0723],["University of Maryland, College Park",38.9869,-76.9426],["University of Reading",51.4414,-0.9418],["University of California, Berkeley",37.8719,-122.2585],["University of California, Riverside",33.9737,-117.3281],["University of Padua",45.4068,11.8774],["Ludwig Maximilian University of Munich",48.1508,11.5804],["Temple University",39.9812,-75.1554],["University of Pittsburgh",40.4444,-79.9608],["Birkbeck, University of London",51.5219,-0.1303],["Georgia Institute of Technology",33.7756,-84.3963],["Ryerson University",43.6577,-79.3788],["University of Oregon",44.0448,-123.0726],["University of New Mexico",35.0843,-106.6198],["University of Portsmouth",50.7951,-1.0936],["Duke University",36.0014,-78.9382],["Stanford University",37.4275,-122.1697],["College of Staten Island, CUNY",40.6021,-74.1504],["University of California, San Diego",32.8801,-117.234],["Gallaudet University",38.9084,-76.9924],["Albert Einstein College of Medicine",40.8521,-73.8444],["Arizona State University",33.4242,-111.9281],["Babes-Bolyai University",46.7678,23.5913],["Boston College",42.3355,-71.1685],["Boston University",42.3505,-71.1054],["Columbia University",40.8075,-73.9626],["Cornell University",42.4534,-76.4735],["East Carolina University",35.6055,-77.3646],["East Tennessee State University",36.3025,-82.3702],["Emory University",33.7925,-84.324],["Florida International University",25.7566,-80.3739],["George Mason University",38.8316,-77.3121],["George Washington University",38.8997,-77.0486],["Harvard University",42.377,-71.1167],["Johns Hopkins University",39.3299,-76.6205],["Lafayette College",40.6986,-75.2087],["Massachusetts Institute of Technology",42.3601,-71.0942],["Montclair State University",40.8645,-74.1986],["North Dakota State University",46.8978,-96.8024],["Northeastern University",42.3398,-71.0892],["Northwestern University",42.0565,-87.6753],["Ohio State University",40.0142,-83.0309],["Sapienza University of Rome",41.9038,12.5144],["Seton Hall University",40.743,-74.2471],["Teachers College, Columbia University",40.8102,-73.9605],["Tufts University",42.4075,-71.119],["University of Arizona",32.2319,-110.9501],["University of British Columbia",49.2606,-123.246],["University of California, Davis",38.5382,-121.7617],["University of California, Los Angeles",34.0689,-118.4452],["University of California, Santa Cruz",36.9881,-122.0582],["University of Connecticut",41.8077,-72.254],["University of Denver",39.6766,-104.9619],["University of Houston",29.7199,-95.3422],["University of Illinois at Chicago",41.8704,-87.6675],["University of Iowa",41.6627,-91.5549],["University of Louisville",38.2123,-85.7585],["University of Massachusetts, Amherst",42.3868,-72.5301],["University of Minnesota",44.974,-93.2277],["University of Missouri, Kansas City",39.0347,-94.5785],["University of Otago",-45.8647,170.5144],["University of Queensland",-27.4975,153.0137],["University of South Carolina",33.9961,-81.0274],["University of Tennessee",35.9544,-83.9295],["University of Toronto",43.6629,-79.3957],["University of Washington",47.6553,-122.3035],["Uppsala University",59.8509,17.63],["Virginia Commonwealth University",37.5488,-77.4527],["Washington University, St Louis",38.6488,-90.3108],["Whitman College",46.0706,-118.3306],["Utrecht University",52.0902,5.1226],["University of Colorado, Boulder",40.0076,-105.2659],["Binghamton University",42.0894,-75.9697],["Lehigh University",40.6069,-75.3783],["University of Akron",41.0767,-81.5113],["Syracuse University",43.0392,-76.1351],["Michigan State University",42.7018,-84.4822],["University of São Paulo",-23.5614,-46.7308],["Lehman College, CUNY",40.8714,-73.8963],["Rosalind Franklin University of Medicine and Science",42.3004,-87.8586],["Swinburne University of Technology",-37.8222,145.039],["Trinity College",41.7479,-72.6903],["Cincinnati Children’s Hospital Medical Center",39.1405,-84.5015],["Macquarie University",-33.7738,151.1126],["Bard College",42.0204,-73.9101],["Oberlin College",41.2935,-82.2236],["University of Hawaii at Manoa",21.2969,-157.8171],["University of Manitoba",49.8075,-97.1366],["Alexandru Ioan Cuza University",47.174,27.5721],["Radboud University",51.8193,5.8569],["Niagara University",43.1365,-79.0353],["Colby College",44.5639,-69.6626],["Wabash College",40.0378,-86.9067],["Wayne State University",42.3591,-83.0665],["McGill University",45.5048,-73.5772],["College of the Holy Cross",42.2392,-71.808],["Queen's University",44.2253,-76.4951],["Wake Forest University",36.1352,-80.2763],["Baylor College of Medicine",29.7105,-95.3962],["ISCTE - University Institute of Lisbon (IUL)",38.7478,-9.1534],["University of Pennsylvania",39.9522,-75.1932],["University of Winchester",51.0597,-1.3282],["Canisius College",42.9248,-78.8515],["University of Central Florida",28.6024,-81.2001],["Grinnell College",41.7491,-92.7201],["Max Planck Institute for Human Development",52.4686,13.3038],["Rochester Institute of Technology",43.0861,-77.6705],["University of Trier",49.7458,6.6884],["Middlesex University",51.5898,-0.2283],["Oregon State University",44.5638,-123.2794],["Claude Bernard University Lyon 1",45.7789,4.868],["Bilkent University",39.8746,32.7476],["Pace University",40.7111,-74.0049],["Purdue University",40.4237,-86.9212],["University of Erlangen-Nuremberg",49.5979,11.0046],["Newcastle University",54.9792,-1.6147],["Williams College",42.7128,-73.203],["University of Missouri, St. Louis",38.7092,-90.3083],["Gettysburg College",39.8362,-77.2375],["Hofstra University",40.7168,-73.5994],["Loyola University Chicago",41.999,-87.6582],["University of Louisiana, Lafayette",30.2114,-92.0204],["University of Nevada, Las Vegas",36.1072,-115.1425],["University of North Carolina, Greensboro",36.0689,-79.8102],["Miami University",39.5105,-84.7309],["Christopher Newport University",37.0646,-76.4944],["Millersville University",39.9977,-76.3544],["University of Nicosia",35.1658,33.3144],["Slippery Rock University",41.063,-80.0412],["Grand Valley State University",42.9639,-85.8889],["Boston Children's Hospital",42.3375,-71.1053],["Wesleyan University",41.5566,-72.6569],["Northern Illinois University",41.9342,-88.7741],["University of the Sciences",39.9468,-75.2071],["Swarthmore College",39.9038,-75.3527],["IE University",40.9528,-4.1188],["Ozyegin University",41.0313,29.2587],["University of Nebraska, Lincoln",40.8202,-96.7005],["Montana State University",45.6668,-111.0498],["Earlham College",39.8209,-84.9131],["Princeton University",40.344,-74.6514],["Rhodes College",35.1551,-89.9893],["Colorado College",38.8466,-104.8244],["University of Arkansas",36.0678,-94.1737],["University of Arkansas at Little Rock",34.7241,-92.3389],["University of Montana",46.8601,-113.9852],["University of Stirling",56.1459,-3.9189],["National Institute of Astrophysics, Optics and Electronics (INAOE)",19.0323,-98.3154],["Memorial University",47.5738,-52.7329],["University of Tampere",61.4937,23.7787],["University of Utah",40.7649,-111.8421],["Central European University",47.5005,19.0496],["Davidson College",35.5017,-80.8468],["University of Geneva",40.7713,-80.3218],["University of North Georgia",34.5279,-83.9844],["Barnard College, Columbia University",40.809,-73.9638],["Free University of Brussels",50.8219,4.3949],["Paul Valéry University,  Montpellier 3",43.6324,3.8702],["Jagiellonian University",50.061,19.9341],["New York University, School of Medicine",40.7421,-73.9739],["Teesside University",54.5707,-1.2353],["Oregon Health & Science University",44.941,-123.0303],["Weill Cornell Medicine",40.7655,-73.955],["University at Buffalo",43.0008,-78.789],["Interdisciplinary Center Herzliya",32.1761,34.8362],["St. Lawrence University",44.5892,-75.1609],["University Paris 8",48.9449,2.3636],["University of the Basque Country",43.3309,-2.9679],["University of California, Merced",37.3661,-120.4224],["Villanova University",40.0371,-75.3436],["Texas Christian University",32.7095,-97.3628],["University of New South Wales",-33.9173,151.2313],["University of Quebec at Trois-Rivieres",46.3472,-72.5771],["Liverpool Hope University",53.3908,-2.8923],["California State University, Northridge",34.241,-118.5277],["University of Oklahoma",35.2059,-97.4457],["University of North Carolina, Asheville",35.6165,-82.5665],["Federal University of Parana",-25.4269,-49.2618],["University of Wisconsin, Whitewater",42.8412,-88.7453],["Queens College, CUNY",40.7366,-73.8201],["Italian Hospital of Buenos Aires",-34.6072,-58.4263],["University of South Florida",28.0587,-82.4139],["Aix-Marseille University",43.2922,5.3591],["Omsk State Pedagogical University",54.9901,73.3577],["University of Angers",47.4771,-0.5499],["San Francisco State University",37.7219,-122.4782],["Federal University of Rio Grande North",-5.8394,-35.2008],["St. John's University",40.7216,-73.7947],["Connecticut College",41.3787,-72.1046],["University of Lisbon",38.7527,-9.1582],["California State University, Stanislaus",37.5252,-120.8554],["Humboldt University of Berlin",52.5179,13.3937],["Albright College",40.3615,-75.9106],["Hangzhou Normal University",30.2895,120.0099],["Leiden University",52.1571,4.4852],["Gustavus Adolphus College",44.3222,-93.9688],["California State University, Long Beach",33.7838,-118.1141],["Providence College",41.8441,-71.4382],["University of Greifswald",54.095,13.3746],["Free University of Berlin",52.4543,13.2935],["Hong Kong University of Science and Technology",22.3364,114.2655],["Old Dominion University",36.8856,-76.3068],["University of Canterbury",-43.5235,172.5839],["University of Texas at Dallas",32.9858,-96.7501],["University of Sussex",50.8671,-0.0879],["University of Verona",45.4374,11.0034],["University of Liverpool",53.4059,-2.9656],["Laboratory of Cognitive and Psycholinguistic Sciences",42.3398,-71.1098],["Tokyo Metropolitan University",35.6172,139.377],["Florida State University",30.4419,-84.2985],["National University of Córdoba",-31.4355,-64.1856],["University of the Pacific",37.9808,-121.312],["Lancaster University",54.0104,-2.7877],["National Scientific and Technical Research Council (CONICET)",-34.5826,-58.4291],["Widener University",39.8634,-75.3567],["California State Polytechnic University, Pomona",34.0589,-117.8194],["University of Alberta",53.5232,-113.5263],["Max Planck Institute for Evolutionary Anthropology",51.3212,12.395],["Skidmore College",43.0973,-73.7842],["Washington State University",46.7319,-117.1542],["University of Mons",50.4588,3.9522],["Clemson University",34.6761,-82.8364],["National Autonomous University of Mexico",19.3189,-99.1844],["Stockholm University",59.3628,18.0593],["University of Los Andes",4.6015,-74.0661],["Concordia University",45.5702,-122.6369],["Duquesne University",40.4373,-79.9902],["University of Tuebingen",48.5295,9.0438],["University of the Incarnate Word",29.4676,-98.4676],["San Jose State University",37.3352,-121.8811],["Boston Medical Center",42.3355,-71.0729],["Niigata University",37.867,138.9425],["University of California, Santa Barbara",34.414,-119.8489],["University of Waterloo",43.4723,-80.5449],["Franklin and Marshall College",40.0475,-76.3179],["Chatham University",40.4482,-79.9243],["Association Italian Teachers Method Feldenkrais (AIIMF)",43.7677,11.2758],["Kutztown University of Pennsylvania",40.5101,-75.7834],["Kobe University",34.7256,135.2354],["University of Western Australia",-31.98,115.819],["University of North Carolina, Chapel Hill",35.9049,-79.0469],["University of Goettingen",51.5408,9.9374],["University of Quebec in Outaouais",45.4225,-75.7387],["Oklahoma State University",36.127,-97.0737],["Institute of Psychology, Chinese Academy of Sciences",40.002,116.3731],["University of New Hampshire",43.1389,-70.937],["University of California, Irvine",33.6405,-117.8443],["Lynchburg College",37.4006,-79.1832],["Charité - University Medicine Berlin",52.5265,13.3766],["University of Wisconsin, Oshkosh",44.0262,-88.5508],["Children's Hospital of Philadelphia (CHOP)",39.9489,-75.194],["University of Illinois at Urbana-Champaign",40.102,-88.2272],["Cardiff University",51.4866,-3.1789],["Brooklyn College, CUNY",40.6305,-73.9521],["Ulster University",55.0064,-7.3244],["Carleton University",45.3876,-75.696],["INSERM",45.7404,4.8931],["Trent University",44.3571,-78.2904],["Wellesley College",42.2936,-71.3059],["University of Florida",29.6436,-82.3549],["London South Bank University",51.4982,-0.1021],["University of Paris Nanterre",48.9034,2.2113],["Ulm University",48.4222,9.9556],["Farmingdale State College",40.7522,-73.4224],["Michigan Technological University",47.1149,-88.5453],["California State University, Fullerton",33.8823,-117.8851],["Pontifical Catholic University of Chile",-33.4418,-70.64],["California Polytechnic State University, San Luis Obispo",35.305,-120.6625],["University of South Dakota",42.7883,-96.9253],["Virginia Military Institute",37.789,-79.4395],["Humboldt State University",40.8753,-124.0778],["Catholic University of America",38.9369,-76.9987],["Marshall University",38.4235,-82.4263],["Pomona College",34.0977,-117.7118],["California State University, San Bernardino",34.1814,-117.3232],["California State University, Sacramento",38.5582,-121.4214],["Australian Catholic University",-27.3779,153.0893],["University of Haifa",32.7614,35.0195],["University of Cambridge",52.2043,0.1149],["Catholic University of Portugal",41.1541,-8.6725],["University of Lincoln",53.2279,-0.5502],["University of Ottawa",45.4231,-75.6831],["Edogawa University",35.8765,139.9387],["University of Algarve",37.044,-7.9722],["Federal University of Alagoas",-9.5546,-35.776],["Roosevelt University",41.8763,-87.6251],["Rikkyo University",35.7305,139.704],["University of the Republic",-34.9011,-56.1733],["University of Trento",46.0668,11.1231],["Saarland University",49.255,7.041],["Oxford Brookes University",51.755,-1.2242],["Technion – Israel Institute of Technology",32.7768,35.0231],["Carlos Albizu University, San Juan",18.4666,-66.1138],["SWPS University of Social Sciences and Humanities",52.2484,21.065],["University of Scranton",41.4067,-75.6586],["Indraprastha Institute of Information Technology Delhi",28.5456,77.2732],["University of Leeds",53.8067,-1.555],["Eastern Michigan University",42.2507,-83.6241],["Children’s Mercy Kansas City",39.0838,-94.5775],["University of Hull",53.7737,-0.3681],["Lock Haven University",41.1423,-77.4621],["Koc University",41.1978,29.0674],["University of California, San Francisco",37.7627,-122.4579],["London School of Economics and Political Science",51.5144,-0.1174],["Simon Fraser University",49.2781,-122.9199],["Adam Mickiewicz University",52.4084,16.9154],["Southern New Hampshire University",43.0408,-71.4532],["Deakin University",-37.8205,144.9502],["Westmont College",34.4488,-119.661],["Copenhagen Business School",55.6816,12.5297],["Texas Woman's University",33.2263,-97.1271],["Max Planck Institute for Psycholinguistics",51.818,5.8571],["University of North Carolina, Wilmington",34.2239,-77.8696],["Sam Houston State University",30.7132,-95.5504],["University of Notre Dame",41.7056,-86.2353],["State University of New York at Fredonia",42.4539,-79.3404],["Thompson Rivers University",50.6725,-120.3706],["Aalto University",60.1867,24.8277],["University of Salford",53.4846,-2.2708],["University Saint Anne",44.3327,-66.1169],["West Virginia University",39.6361,-79.9559],["Curtin University",-32.0062,115.8944],["Banja Luka University",44.7745,17.2112],["DePaul University",41.9256,-87.655],["New College of Florida",27.3848,-82.5587],["Adelphi University",40.7197,-73.6517],["General Hospital of Mexico",39.1725,-91.8773],["University of Wisconsin, Milwaukee",43.0783,-87.882],["Academic Medical Center",52.2946,4.958],["University of East London",51.5076,0.0651],["University of Campinas",-22.8184,-47.0647],["Sacred Heart University",41.2204,-73.2433],["Amherst College",42.3709,-72.517],["National Taipei University of Technology",25.0422,121.5355],["University of Warsaw",52.2403,21.0186],["Colorado State University",40.5734,-105.0865],["University of Toledo",41.658,-83.6141],["Santa Clara University",37.3496,-121.939],["University of Muenster",51.9636,7.6133],["Hunter College, CUNY",40.7678,-73.9645],["Kyushu University",33.6267,130.425],["Kumamoto University",32.814,130.7279],["Worcester Polytechnic Institute",42.2746,-71.8063],["University of East Anglia",52.6219,1.2392],["University of Nantes",47.2095,-1.556],["Osaka University",34.822,135.5245],["University Putra Malaysia",2.9993,101.7079],["Istanbul Technical University",41.1056,29.0253],["National University of La Plata",-34.9128,-57.9512],["Clark University",42.2523,-71.8247],["Trinity College Dublin",53.3438,-6.2546],["Baruch College, CUNY",40.7404,-73.9832],["Graduate Center, CUNY",40.7486,-73.984],["Bethel College",38.0737,-97.3433],["University of St Andrews",56.3417,-2.7928],["Randolph-Macon College",37.7604,-77.4784],["Wilfrid Laurier University",43.4724,-80.5263],["San Diego State University",32.7757,-117.0719],["York University",43.7735,-79.5019],["King's University College at the University of Western Ontario",43.0125,-81.2571],["University of San Francisco",37.7766,-122.4507],["Mercy College",41.0221,-73.8745],["Nationwide Children's Hospital",39.9531,-82.9796],["Hartwick College",42.4587,-75.0718],["Macalester College",44.9379,-93.1691],["Victoria University of Wellington",-36.8483,174.7677],["University of Regina",50.417,-104.5885],["University of Magdeburg",52.1397,11.6474],["National University of Singapore",1.2966,103.7764],["William Paterson University",40.9474,-74.1967],["Pontifical Catholic University of Rio Grande do Sul",-30.0589,-51.173],["University of Turku",60.4563,22.2851],["University of Konstanz",47.6894,9.1869],["Metropolitan State University of Denver",39.7432,-105.0063],["New School University",40.7353,-73.9938],["University of York",53.9455,-1.0562],["University of Guadalajara",20.6752,-103.359],["Chinese University of Hong Kong",22.4196,114.2068],["Federal University of Minas Gerais",-19.8707,-43.9677],["University of Potsdam",52.401,13.0119],["Bogazici University",41.0848,29.051],["University of Memphis",35.1187,-89.9371],["College of Idaho",43.6537,-116.6768],["University of Auckland",-36.8523,174.7691],["University of L'Aquila",42.3514,13.3973],["Kansas State University",39.1974,-96.5847],["Nanyang Technological University",1.3483,103.6831],["University of Belgrade",44.8185,20.4576],["The University of Hong Kong",22.283,114.1371],["University of North Florida",30.2661,-81.5072],["Medical University of South Carolina",32.7847,-79.9509],["National Acoustic Laboratories",40.1365,-105.1261],["Ruhr University Bochum",51.4457,7.2616],["Gordon College",33.0469,-84.1551],["University of Hamburg",53.5666,9.9846],["Bundeswehr University Munich",48.0803,11.6382],["University of Neuchatel",46.9939,6.9387],["Texas State University",29.8884,-97.9384],["Wingate University",34.9859,-80.4431],["Trinity University",29.4618,-98.4833],["Hunter New England Health",42.1017,-72.5909],["Federal University of Paraiba",-7.1378,-34.8459],["College at Brockport, SUNY",43.21,-77.9507],["Politehnica University of Bucharest",44.4386,26.0495],["University of Campania Luigi Vanvitelli",41.0667,14.3305],["University of Kentucky",38.0307,-84.504],["Amsterdam University College",52.3549,4.9512],["Hampshire College",42.3256,-72.5313],["Reed College",45.4811,-122.6308],["Emirates College for Advanced Education",24.4204,54.5965],["Yeshiva University",43.7697,-79.4675],["University of Oslo",59.94,10.7217],["University of Alabama",33.214,-87.5391],["Illinois State University",40.512,-88.9932],["Central Michigan University",43.5819,-84.7756],["University College London",51.5246,-0.134],["Ben-Gurion University of the Negev",31.2622,34.8015],["Australian National University",-35.2777,149.1185],["Okayama University",34.6871,133.9222],["Denison University",40.0734,-82.5229],["Missouri State University",37.2006,-93.2807],["Saint Vincent College",40.2989,-79.3995],["Truman State University",40.1832,-92.581],["Bradley University",40.6978,-89.6153],["Dickinson College",40.2027,-77.2008],["Duke Kunshan University",31.416,120.9014],["Anna Maria College",42.3293,-71.9208],["Misericordia University",41.3452,-75.9707],["Carthage College",42.6222,-87.822],["Augsburg University",44.9659,-93.2407],["Strategic Education Research Partnership (SERP) Institute",38.904,-77.0406],["Georgia College & State University",33.0804,-83.2319],["Indiana Wesleyan University",40.517,-85.6626],["Loyola University Maryland",39.3464,-76.621],["University of Brest",48.398,-4.5076],["University of Wisconsin, Stout",44.8716,-91.9267],["Central New Mexico College",35.0714,-106.6289],["Spokane Community College",47.6746,-117.3597],["The University of Edinburgh",55.9445,-3.1892],["Union College",42.8177,-73.9296],["Colombian School of Engineering Julio Garavito",4.7827,-74.0426],["University of Wisconsin, Parkside",42.645,-87.8517],["Longwood University",37.2972,-78.3962],["Florida Atlantic University",26.375,-80.1011],["Drew University",40.7607,-74.4274],["Sidra Medicine",40.7074,-73.6981],["Pablo de Olavide University, Seville",37.3552,-5.9374],["College of Southern Nevada",36.0082,-114.9654],["James Madison University",38.4351,-78.8698],["PES University",12.9354,77.5358],["Maryville University",38.6449,-90.5046],["University of Baltimore",39.3059,-76.6175],["University of Nottingham",52.9398,-1.193],["University of Mississippi",34.3647,-89.5384],["University of Zurich",47.3743,8.551],["Utah State University",41.7452,-111.8097],["Indiana University, South Bend",41.6623,-86.2197],["Mercer University",33.8729,-84.2632],["MedStar Health Research Institute",38.9696,-76.9532],["Saint Joseph's University",39.9947,-75.2416],["Viterbo University",43.8021,-91.2451],["Drexel University",39.9566,-75.1899],["Columbus State University",32.5026,-84.9404],["Methodist University",35.1358,-90.0195],["High Point University",35.9722,-79.9952],["Brevard College",35.2367,-82.7295],["King's College London",51.5115,-0.116],["University of Granada",37.1838,-3.6007],["Texas Tech University",33.5843,-101.8783],["Pacific Lutheran University",47.1452,-122.4437],["Federal University of Santa Catarina",-27.6,-48.5194],["Massachusetts General Hospital",42.3626,-71.0676],["California Institute of Integral Studies",37.7747,-122.4164],["University of Missouri, Columbia",38.9404,-92.3277],["Keimyung University",35.8535,128.4799],["SRI International",38.8946,-77.0702],["University of Newcastle",42.5122,-92.4646],["SickKids",43.6573,-79.3873],["Keil University",54.3461,10.1147],["Pamukkale University",37.7382,29.1039],["Portland State University",45.5111,-122.6834],["New University of Lisbon",38.7337,-9.1607],["University Health Network",43.6599,-79.3876],["Muhlenberg College",40.5975,-75.5101],["Dominican College",41.0472,-73.9506],["Dalton State College",34.7734,-85.0031],["Spring Hill College",30.6919,-88.1357],["Spelman College",33.7452,-84.4114],["University of Massachusetts, Dartmouth",41.6282,-71.0046],["SUNY College at Old Westbury",40.7993,-73.574],["Middle Tennessee State University",35.8486,-86.3649],["Osaka City University, Graduate School of Medicine",34.5906,135.5051],["Ohio State University, Lima",40.7373,-84.0283],["Pennsylvania State University, Worthington-Scranton",41.4402,-75.6205],["Pennsylvania State University, Brandywine",39.9264,-75.4471],["Pennsylvania State University, Applied Research Laboratory",40.7929,-77.866],["Pennsylvania State University, Abington",40.1153,-75.1106],["Kwantlen Polytechnic University",49.1327,-122.8715],["Assumption College",42.2927,-71.829],["Haverford College",40.0093,-75.3077],["MGH Institute of Health Professions",42.3748,-71.0542],["Carroll University",43.0044,-88.2284],["The University of Sydney",-33.8886,151.1873],["Robert Bosch Hospital",48.8148,9.1872],["Western Oregon University",44.8536,-123.2388],["University of Kent",51.2985,1.071],["University of North Carolina, Charlotte",35.3071,-80.7352],["Presbyterian College",34.4652,-81.8776],["University of Colorado, Denver",39.7464,-105.0023],["Marywood University",41.4351,-75.6324],["Rowan University",39.7089,-75.1183],["The Hebrew University of Jerusalem",31.7946,35.2414],["Emerson College",42.3522,-71.0656],["Tilburg University",51.5639,5.0434],["University of Plymouth",50.3762,-4.1395],["Augustana College",41.503,-90.5514],["University of Osnabrueck",52.2713,8.0442],["Brock University",43.1176,-79.2477],["University of North Texas",33.2075,-97.1526],["Rutgers University, Camden",39.9475,-75.1232],["University of Pecs",46.0742,18.2377],["Meritorious Autonomous University of Puebla",19.0387,-98.2148],["Karolinska Institute",59.3481,18.0237],["Saint Anselm College",42.9838,-71.5072],["Chapman University",33.7931,-117.8521],["Bielefeld University",52.0367,8.4952],["University of Colorado, Colorado Springs",38.8966,-104.8049],["Baylor University",31.5489,-97.1131],["South China University of Technology",23.1513,113.3447],["Belmont University",36.1329,-86.7942],["Queen Mary University of London",51.5241,-0.0404],["University of Louisiana, Monroe",32.5267,-92.0732],["Elon University",36.1027,-79.5023],["University of Zaragoza",41.6421,-0.9015],["Tel Aviv University",32.1133,34.8044],["Birmingham-Southern College",33.5159,-86.851],["University of Applied Sciences, Potsdam",52.4126,13.0508],["Western University",43.0096,-81.2737],["Amrita Vishwa Vidyapeetham",10.9027,76.9006],["Indiana University, Purdue University Indianapolis",39.7743,-86.1764],["National School of Engineers of Carthage (ENICarthage)",36.8518,10.2111],["University of Milano-Bicocca",45.5184,9.2131],["Athens State University",34.8056,-86.9645],["Borough of Manhattan Community College, CUNY",40.7189,-74.0118],["Avila University",38.9124,-94.5915],["Kyoto University",35.0262,135.7808],["University of Bologna",44.4962,11.3542],["Taif University",21.4328,40.4919],["University of Strathclyde",55.8621,-4.2424],["Mount Holyoke College",42.2554,-72.5722],["American University",38.9375,-77.0888],["University of Antwerp",51.2228,4.4102],["Auburn University",32.5934,-85.4952],["University of Nebraska, Omaha",41.258,-96.0107],["Iowa State University",42.0267,-93.6465],["University of Maryland, Baltimore County",39.2557,-76.711],["Ohio Dominican University",39.9922,-82.9416],["Queen's University, Belfast",54.5844,-5.934],["Open University",52.0252,-0.709],["St. Olaf College",44.4617,-93.1827],["University of Montreal",45.5056,-73.6138],["Anglia Ruskin University",52.2033,0.1345],["Arctic University of Norway",69.6798,18.9712],["National Taiwan University",25.0173,121.5398],["University of Chester",53.2003,-2.8991],["University of Girona",41.9857,2.8272],["National Institute for Basic Biology",34.9494,137.1652],["Palo Alto University",37.3825,-122.1874],["Jissen Women's University",35.6716,139.3898],["University of Texas at Tyler",32.3157,-95.2542],["Kingston University London",51.4032,-0.3035],["MEF University",41.1088,29.0086],["American University of Beirut",33.9008,35.4807],["Bar-Ilan University",32.0692,34.8431],["East China Normal University",31.2277,121.4068],["University of Tehran",35.7022,51.3957],["Brigham Young University, Idaho",43.8144,-111.7833],["University of Nevada, Reno",39.5453,-119.8162],["Sri Lanka Institute of Information Technology",6.9147,79.9729],["Clarkson University",44.6637,-74.9977],["University of Manchester",53.4668,-2.2339],["Brunel University London",51.5324,-0.473],["Pontifical Catholic University of Rio de Janeiro",-22.9795,-43.2329],["Worcester State University",42.2678,-71.8441],["University of Applied Sciences, Jena",50.9183,11.5687],["Wofford College",34.9612,-81.9346],["University of Northern Colorado",40.4033,-104.7002],["Leiden University Medical Center",52.166,4.4785],["Wheaton College",41.8683,-88.0996],["Weber State University",41.1916,-111.9441],["California Lutheran University",34.2249,-118.8789],["University of British Columbia, Okanagan",49.9423,-119.396],["Northeastern Illinois University",41.9804,-87.7187],["Rollins College",28.5928,-81.3508],["Ripon College",43.8432,-88.8408],["Maastricht University",50.8393,5.7088],["California State University, Fresno"],["Kessler Foundation"]];
