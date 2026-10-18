Python 3.14.0 (tags/v3.14.0:ebf955d, Oct  7 2025, 10:15:03) [MSC v.1944 64 bit (AMD64)] on win32
Enter "help" below or click "Help" above for more information.
>>> import subprocess
... import os
... 
... print("--- Starting Push-Button Replication Workflow ---")
... 
... # 1. Run Card & Krueger Replication Script
... print("Executing Card & Krueger Replication...")
... subprocess.run(["Rscript", "code/Replication.R"], check=True)
... 
... # 2. Run SCM Extension Script
... print("Executing Synthetic Control Extension...")
... subprocess.run(["Rscript", "code/SCM_722513.R"], check=True)
... 
