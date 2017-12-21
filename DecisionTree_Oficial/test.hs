import Data.DecisionTree

decideProp = (map (decide tree) unlabeled) == labels where
    tree = build atts dataset


outlook = A { aName = "outlook", possibleValues=["sunny", "overcast", "rainy"] }
temperature = A { aName ="temperature", possibleValues=["hot","mild", "cool"] }
humidity = A { aName ="humidity", possibleValues=["high","normal"] }
windy = A { aName ="windy", possibleValues=["true","false"]}

atts = [outlook,temperature,humidity,windy]

dataset = zip labels unlabeled
labels = ["no","no","yes","yes","yes","no","yes","no","yes","yes","yes","yes","yes","no"]
unlabeled = 
    [D { dName = "", attributes = [(outlook,"sunny"), 
            (temperature,"hot"), 
            (humidity,"high"),
            (windy,"false")]},
    D { dName = "", attributes = [(outlook,"sunny"), 
        (temperature,"hot"), 
        (humidity, "high"), 
        (windy,"true")]},
    D { dName = "", attributes = [(outlook,"overcast"),
        (temperature,"hot"), 
        (humidity, "high"), 
        (windy, "false")]},
    D { dName = "", attributes = [(outlook,"rainy"), 
        (temperature,"mild"), 
        (humidity, "high"), 
        (windy,"false")]},
    D { dName = "", attributes = [(outlook,"rainy"), 
        (temperature, "cool"), 
        (humidity, "normal"), 
        (windy,"false")]},
    D { dName = "", attributes = [(outlook, "rainy"), 
        (temperature,"cool"), 
        (humidity, "normal"), 
        (windy, "true")]},
    D { dName = "", attributes = [(outlook, "overcast"), 
        (temperature, "cool"), 
        (humidity, "normal"), 
        (windy,"true")]},
    D { dName = "", attributes = [(outlook, "sunny"), 
        (temperature, "mild"), 
        (humidity, "high"), 
        (windy, "false")]},
    D { dName = "", attributes = [(outlook, "sunny"), 
        (temperature, "cool"), 
        (humidity, "normal"), 
        (windy, "false")]},
    D { dName = "", attributes = [(outlook, "rainy"), 
        (temperature, "mild"), 
        (humidity, "normal"), 
        (windy, "false")]},
    D { dName = "", attributes = [(outlook, "sunny"), 
        (temperature, "mild"), 
        (humidity, "normal"), 
        (windy, "true")]},
    D { dName = "", attributes = [(outlook, "overcast"), 
        (temperature, "mild"), 
        (humidity, "high"), 
        (windy, "true")]},
    D { dName = "", attributes = [(outlook, "overcast"), 
        (temperature, "hot"), 
        (humidity, "normal"), 
        (windy, "false")]},
    D { dName = "", attributes = [(outlook, "rainy"), 
        (temperature, "mild"), 
        (humidity, "high"), 
        (windy,"true")]}]

