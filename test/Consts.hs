module Consts where

import Test.QuickCheck

minLitBound :: Int
minLitBound = -100

maxLitBound :: Int
maxLitBound = 100

maxTupleSize :: Int 
maxTupleSize = 5

maxListSize :: Int
maxListSize = 10

maxParamSize :: Int
maxParamSize = 5

maxConstrSize :: Int 
maxConstrSize = 5

maxCaseAltSize :: Int
maxCaseAltSize = 5

maxDeclSize :: Int 
maxDeclSize = 5

varNames = ["x", "y", "z", "a", "b", "c", "n", "m", "k", "i", "j", "s", "t", 
            "f", "g", "h", "l", "r", "u", "v", "w", "e", "d", "p", "q", "xs", 
            "ys", "zs", "as", "bs", "cs", "ns", "ms", "ks", "is", "js", "ss", 
            "ts", "fs", "gs", "hs", "ls", "rs", "us", "vs", "ws", "es", "ds", 
            "ps", "qs", "x1", "x2", "x3", "y1", "y2", "y3", "z1", "z2", "z3", 
            "a1", "a2", "a3", "b1", "b2", "b3", "c1", "c2", "c3", "n1", "n2", 
            "n3", "m1", "m2", "m3", "k1", "k2", "k3", "i1", "i2", "i3", "j1", 
            "j2", "j3", "s1", "s2", "s3", "t1", "t2", "t3", "f1", "f2", "f3", 
            "g1", "g2", "g3", "h1", "h2", "h3", "l1", "l2", "l3", "r1", "r2", 
            "r3", "u1", "u2", "u3", "v1", "v2", "v3", "w1", "w2", "w3", "e1", 
            "e2", "e3", "d1", "d2", "d3", "p1", "p2", "p3", "q1", "q2", "q3"]

funNames = ["print", "input", "len", "range", "int", "float", "str", "bool", 
            "list", "dict", "set", "tuple", "type", "id", "dir", "help", 
            "abs", "max", "min", "sum", "sorted", "reversed", "map", "filter", 
            "reduce", "zip", "enumerate", "iter", "next", "open", "close", 
            "read", "write", "append", "seek", "tell", "format", "join", 
            "split", "replace", "find", "index", "count", "upper", "lower", 
            "capitalize", "title", "strip", "lstrip", "rstrip", "isdigit", 
            "isalpha", "isalnum", "isspace", "startswith", "endswith", "in", 
            "import", "from", "def", "return", "lambda", "if", "else", "elif", 
            "for", "while", "break", "continue", "pass", "assert", "raise", "try", 
            "except", "finally", "with", "as", "class", "object", "self", "super", 
            "init", "del", "repr", "str", "eq", "ne", "lt", "le", "gt", "ge", 
            "add", "sub", "mul", "truediv", "floordiv", "mod", "pow", "and", "or", 
            "not", "is", "isnot", "global", "nonlocal", 
            "yield", "async", "await", "awaitable", "coroutine", "generator", 
            "decorator", "annotation", "docstring", "module", "package", "script", 
            "namespace", "scope", "variable", "function", "method", "attribute", 
            "property", "parameter", "argument", "keyword", "operator", 
            "expression", "statement", "block", "indentation", "comment", 
            "string", "number", "integer", "float", "complex", "boolean", 
            "list", "dictionary", "set", "tuple", "slice", "index", "key", 
            "value", "item", "element", "iterable", "iterator", "sequence", 
            "collection", "container", "mutable", "immutable", "hashable", 
            "comparable", "sortable", "callable", "returnable", "subscriptable", 
            "unpackable", "comprehensible", "exception", "error", "warning", 
            "traceback", "stack", "frame", "code", "object", "byte", "bytecode", 
            "source", "file", "path", "name", "alias", "import", "from", "as", 
            "sys", "os", "math", "random", "datetime", "time", "re", "json", 
            "csv", "xml", "html", "requests", "urllib", "socket", "threading", 
            "multiprocessing", "subprocess", "queue", "asyncio", "tkinter", 
            "pygame", "numpy", "pandas", "matplotlib", "scipy", "sklearn", 
            "tensorflow", "pytorch", "nltk", "spacy", "gensim", "flask", 
            "django", "sqlalchemy", "sqlite3", "mysql", "postgresql", "mongodb",
            "redis", "kafka", "rabbitmq", "celery", "docker", "kubernetes", 
            "ansible", "git", "github", "jupyter", "vscode", "pycharm", "pylint", 
            "pytest", "unittest", "doctest", "sphinx", "setuptools", "pip", 
            "virtualenv", "conda", "poetry", "pypi", "anaconda", "colab",             
            "kaggle", "binder", "heroku", "aws", "azure", "gcp", "ibm", 
            "oracle", "digitalocean", "linode", "namecheap", "godaddy", 
            "cloudflare", "letsencrypt", "ssl", "tls", "http", "https", 
            "ftp", "ssh", "scp", "sftp", "telnet", "ping", "traceroute", 
            "nslookup", "whois", "curl", "wget", "postman", "soap", "rest", 
            "graphql", "api", "web", "server", "client", "browser", "chrome", 
            "firefox", "edge", "safari", "opera", "ie", "html", "css", "js", 
            "jquery", "bootstrap", "react", "angular", "vue", "node", "express", 
            "php", "laravel", "ruby", "rails", "java", "spring",
            "go", "rust", "swift", "kotlin", "dart", "flutter", "r", "shiny", 
            "sql", "nosql", "xml", "yaml", "toml", "ini", "csv", "tsv", "pdf", 
            "docx", "xlsx", "pptx", "txt", "md", "rst", "tex", "latex", "epub", 
            "mobi", "azw", "mp3", "mp4", "avi", "mkv", "jpg", "png", "gif", "svg", 
            "bmp", "tiff", "webp", "zip", "rar", "tar", "gz", "bz2", "xz", 
            "iso", "exe", "dll", "so", "dylib", "py", "pyd", "pyc", "pyo", "ipynb", 
            "js", "jsx", "ts", "tsx", "cs", "csx", "java", "class", "jar", "war", 
            "go", "rb", "pl", "pm", "sh", "bash", "zsh", "fish", "csh", "tcsh", 
            "ksh", "awk", "sed", "grep", "egrep", "fgrep", "cut", "paste", "sort", 
            "uniq", "wc", "head", "tail", "cat", "tac", "more", "less", "nano", 
            "vi", "vim", "emacs", "ed", "diff", "patch", "make", "cmake", "rake", 
            "ant", "maven", "gradle", "sbt", "npm", "yarn", "webpack", "gulp", 
            "grunt", "bower", "composer", "gem", "cargo", "pip", "easyinstall", 
            "pipenv", "venv", "virtualenv", "conda", "poetry", "flit", "flake8", 
            "black", "isort", "yapf", "autopep8", "pylint", "pyflakes", 
            "pycodestyle", "pep8", "pep257", "pydocstyle", "bandit", "mypy", 
            "pytype", "pyright", "pyre", "jedi", "rope", "pytest", "unittest", 
            "doctest", "nose", "nosetests", "pytestcov", "coverage", "hypothesis",
            "tox", "nox", "travis", "appveyor", "circleci", "githubactions", 
            "azuredevops", "gitlabci", "jenkins", "codeship", "codecov", 
            "coveralls", "codacy", "sonarqube", "sonarcloud", "landscape", 
            "prospector", "radon", "wily", "vulture", "pyroma", "interrogate", 
            "checkmanifest", "twine", "setuptools", "wheel", "distutils", 
            "pbr", "scikitbuild", "cffi", "cython", "pybind11", "numpy", 
            "scipy", "pandas", "matplotlib", "seaborn", "plotly", "bokeh", 
            "altair", "holoviews", "geopandas", "shapely", "folium", "basemap", 
            "cartopy", "pyproj", "rasterio", "gdal", "fiona", "ogr", 
            "rasterstats", "zarr", "xarray", "dask", "modin", "ray", "vaex", 
            "rapids", "cudf", "cuml", "cupy", "numba", "pytorch", "tensorflow", 
            "keras", "mxnet", "jax", "flax", "pyro", "edward", "pymc3", "emcee", 
            "gpytorch", "gpflow", "sklearn", "statsmodels", "patsy", "lifelines", 
            "linearmodels", "pygam", "glmnet", "xgboost", "lightgbm", "catboost", 
            "ngboost", "shap", "lime", "eli5", "mlflow", "dvc", "kedro", "luigi", 
            "airflow", "prefect", "dagster", "dbt", "greatexpectations", "feast", 
            "tfx", "kubeflow", "seldon", "bentoML", "clipper", "cortex", "fastapi", 
            "flask", "django", "starlette", "uvicorn", "gunicorn", "waitress", 
            "sanic", "tornado", "aiohttp", "requests", "urllib", "httpx", 
            "scrapy", "beautifulsoup", "lxml", "html5lib", "selenium", "playwright", 
            "pyppeteer", "splash", "mechanicalsoup", "robobrowser", "feedparser", 
            "newspaper", "readability", "goose", "textract", "pdfminer", "pypdf2", 
            "camelot", "tabula", "pdftotext", "tika", "fitz", "ghostscript", 
            "poppler", "imagemagick", "pillow", "opencv", "scikitimage", "mahotas", 
            "simplecv", "pytesseract", "pyocr", "wand", "ocrmypdf", "pdfplumber", 
            "nltk", "spacy", "gensim", "textblob", "polyglot", "flair", "transformers", 
            "allennlp", "stanza", "corenlp", "opennlp", "maltparser", "udpipe", 
            "fasttext", "word2vec", "glove", "elmo", "bert", "gpt", "gpt2", "gpt3", 
            "xlnet", "albert", "roberta", "distilbert", "bart", "t5", "electra", 
            "reformer", "bigbird", "longformer", "pegasus", "megatron", 
            "deepspeed", "fairseq", "huggingface", "sentencepiece", "tokenizers", 
            "sacremoses", "moses", "subwordnmt", "bpe", "bpedropout", "unmt", "xnmt", 
            "opennmt", "fairseq", "sockeye", "joeynmt", "marian", "bergamot", 
            "ctranslate2", "tensor2tensor", "t2t", "trax", "meshtensorflow", 
            "lingvo", "espnet", "kaldi", "wav2letter", "wav2vec", "deepspeech", 
            "vosk", "speechbrain", "pyaudio", "sounddevice", "soundfile", "librosa", 
            "pydub", "pysndfile", "audioread", "aubio", "essentia", "madmom", 
            "crepe", "spleeter", "pyworld", "pysptk", "torchaudio", 
            "tensorflowio", "kapre", "nnAudio", "nnabla", "julius", "pocketsphinx", 
            "cmusphinx", "sphinx4", "festival", "flite", "espeak", "pyttsx", "gTTS", 
            "pico2wave", "marytts", "mimic", "tacotron", "tacotron2", "fastspeech", 
            "fastspeech2", "glowtts", "waveglow", "wavernn", "waveflow", "wavegan", 
            "wavenet", "parallelwavenet", "melgan", "multibandmelgan", "pwgan", 
            "mbmelgan", "hifigan", "wavegrad", "wave2vec", "lpcnet", "rnnoise", 
            "speex", "opus", "silk", "amr", "amrwb", "amrnb", "gsm", "ilbc", 
            "codec2", "flac", "vorbis", "opus", "mp3", "aac", "ac3", "wma", "alac", 
            "wavpack", "ape", "mpc", "tta", "ofr", "shn", "dsd", "dff", "dsf", 
            "sacd", "iso"]

dataNames = ["Apple", "Beauty", "Dog", "Pain", "School", "Fire", "Cat", "Humor",
             "Island", "Justice", "Kiwi", "Light", "Table", "Cloud", "Glasses", 
             "Peace", "Cheese", "Laughter", "Frog", "Time", "Grape", "Life", 
             "Chess", "Zebra", "Water", "Ball", "House", "Dice", "Energy", 
             "Knife", "Ice", "Story", "Image", "Game", "Pencil", "Music", 
             "Christmas", "Egg", "Bread", "Picture", "Mouse", "Shadow", "Earth", 
             "Bear", "Wind", "Cup", "Zero", "Anchor", "Cake", "Bed", "Tooth", 
             "Star", "Party", "Grass", "Idea", "Window", "Lake", "Magic", 
             "Night", "Eye", "Paper", "Puzzle", "Wheel", "Sun", "Theater", 
             "Nail", "Trip", "Syrup", "Zoo", "Tree", "Bicycle", "Coffee", 
             "Money", "Mirror", "Flower", "Bottle", "Church", "Garden", "Book", 
             "Machine", "Nose", "Ear", "Wall", "Room", "Clock", "Shoe", "Phone", 
             "Belly", "Volcano", "Shawl", "Zipper", "Algebra", "Flag", "Sky", 
             "Drawing", "Sport", "Cold", "Sunflower", "Winter", "Knee", "Orange", 
             "Map", "Knot", "Wave", "Piano", "Burn", "Radio", "Salt", "Cutlery", 
             "Uniform", "Glass", "Xerox", "Buzz", "Love", "Butterfly", "Heart", 
             "Doubt", "Hope", "Hunger", "Taste", "Envy", "Way", "Freedom", "Fear", 
             "Name", "Gold", "Word", "Quality", "Anger", "Longing", "Boredom", 
             "Union", "Will", "Checkmate", "Care", "Joy", "Play", "Affection", 
             "Desire", "Education", "Future", "War", "Childhood", "Youth", "Trash", 
             "Lie", "Opportunity", "Poetry", "Quantity", "Rule", "Health", 
             "Sadness", "Universe", "Truth", "Chess", "Zen"]

phrases = ["Hello world", "Good morning", "Happy birthday", "Thank you", 
           "Nice to meet you", "How are you", "What's up", "See you later", 
           "Take care", "Have fun", "I'm sorry", "No problem", "Good luck", 
           "Congratulations", "Well done", "I love you", "I miss you", 
           "I need you", "You're welcome", "Please help", "Call me", 
           "Follow me", "Trust me", "Forgive me", "Forget me", "Let's go", 
           "Come here", "Stay there", "Be careful", "Watch out", 
           "Look at this", "Listen to me", "Talk to me", "Tell me", 
           "Show me", "Give me", "Take this", "Keep that", "Throw it away", 
           "Pick it up", "Put it down", "Turn it on", "Switch it off", 
           "Open the door", "Close the window", "Lock the gate", 
           "Unlock the car", "Start the engine", "Stop the music", 
           "Play the game", "Pause the video", "Resume the podcast", 
           "Rewind the tape", "Fast forward the movie", "Record the sound", 
           "Delete the file", "Save the document", "Print the page", 
           "Scan the code", "Copy the text", "Paste the link", 
           "Cut the paper", "Draw the picture", "Paint the wall", 
           "Write the letter", "Read the book", "Sing the song", 
           "Dance the night away", "Laugh out loud", "Cry a river", 
           "Smile more", "Frown less", "Dream big", "Work hard", "Relax more", 
           "Stress less", "Eat well", "Drink water", "Sleep tight", "Wake up", 
           "Get up", "Sit down", "Stand up", "Walk away", "Run faster", 
           "Jump higher", "Fly higher", "Swim deeper", "Dive deeper", 
           "Climb higher", "Fall lower", "Rise again", "Shine brighter", 
           "Grow stronger", "Learn more", "Teach better", "Live longer", 
           "Love deeper", "Be happier", "The sky is blue", "I like pizza", 
           "You are amazing", "She is beautiful", "He is smart", 
           "They are happy", "We are friends", "It is raining", "This is fun", 
           "That is funny", "What is that", "Who are you", "Where are we", 
           "When is it", "Why is this", "How do you", "Do you know", 
           "Can you help", "Will you come", "Would you like", "Could you please", 
           "Should we go", "Are you ready", "Is this yours", "Was that him", 
           "Were they there", "Have you seen", "Has he done", "Had she said", 
           "Did you hear", "Does it work", "Do not worry", "Be yourself", 
           "Try your best", "Learn something new", "Teach me something", 
           "Make me laugh", "Tell me a joke", "Sing me a song", "Write me a poem", 
           "Draw me a picture", "Paint me a portrait", "Give me a hug", 
           "Take me a photo", "Show me a magic", "Play me a game", "Cook me a meal", 
           "Bake me a cake", "Knit me a scarf", "Sew me a dress", "Build me a house", 
           "Plant me a tree", "Grow me a flower", "Find me a treasure", 
           "Solve me a puzzle", "Create me a list", "Generate me a code", 
           "Design me a logo", "Invent me a gadget", "Discover me a planet", 
           "Explore me a cave", "Travel me a world", "Fly me a kite", 
           "Drive me a car", "Ride me a bike", "Swim me a lake", 
           "Sail me a boat", "Climb me a mountain", "Hike me a trail", 
           "Run me a marathon", "Jump me a rope", "Dance me a salsa", 
           "Skate me a ice", "Ski me a snow", "Surf me a wave", 
           "Golf me a hole", "Tennis me a match", "Basketball me a hoop", 
           "Football me a goal", "Chess me a checkmate", "Poker me a flush", 
           "Monopoly me a hotel", "Scrabble me a word", "Sudoku me a grid", 
           "Crossword me a clue", "Jigsaw me a piece", "Rubik me a cube", 
           "Lego me a brick", "Barbie me a doll", "Pokemon me a pikachu", 
           "Mario me a mushroom", "Zelda me a triforce", 
           "Minecraft me a diamond", "Fortnite me a victory", 
           "Among me a impostor", "Candy me a crush"]

