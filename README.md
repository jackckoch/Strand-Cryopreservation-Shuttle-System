# Strand-Cryopreservation-Shuttle-System
Dataset and R code for Aplysia egg strand time study comparing improvised and open-hardware devices.

[1] AplysiaEM_Sizing_Packaging.R: R file including all code for analyses and graphs

[2] cutting_data.csv: Dataset from time study comparing use of improvised tape guide "ruler" and Strand Centi-Sizer "Aplysia Bar" to cut untangled egg strand pieces into 1-cm strands.

    eggmass = egg mass number as assigned by the National Resource for Aplysia
    date = date of data collection
    session = operator session replicate
    operator = cutting operator
    device = cutting device; ruler = tape guide; Aplysia Bar = Strand Centi-Sizer
    time.sec = time in seconds for strand piece
    count = number of 1-cm strands generated from 1 untangled strand piece
    curly = notation of difficult pieces of egg strand

[3] processing_data.csv: Dataset from time study comparing use of improvised "load-and-plunge" method and Strand-Straw Cassette System to load, package, and seal strands into ten 0.5-mL French straws.

    time.sec = time in seconds to complete step
    straw.num = number of straws used for task replicate
    strand.num = number of strands used for task replicate
    operator = operator performing steps
    operation = operation in load-and-plunge or cassette methods
    step = classification of operation
    group = load-and-plunge or cassette methods classification
    avg.group = classification for calculating averages in R
    operation2 = combines two steps in the cassette method for better comparison                     with load-and-plunge method

[4] untangling_data.csv: Dataset from time study comparing time to untangle egg masses based on difficulty of egg mass tangle.

    eggmass = egg mass number as assigned by the National Resource for Aplysia
    date = date of data collection
    session.number = operator session replicate
    operator = untangling operator
    percieved.difficulty = difficulty of tangle of egg mass; easy = easy, medium =                             medium, high = difficult
    time.sec = time in seconds for untangling 
    count = number of 1-cm egg strands that resulted from the length of egg strands             that was untangled
