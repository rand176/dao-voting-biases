import pandas as pd

import json
import os


## Utility functions to read/load files.
########################################

def pd_read_json(file):
    ## Prevents Value too big Error.
    with open(file) as f:
        df = json.load(f)
    df = pd.DataFrame(df)
    return df

def pd_read_dir(dir, blacklist=None, whitelist=None,
                ext=('.json', '.csv', '.csv.gz'), count_only=False,
                callback=None, verbose=False):

    counter = 0
    dir_df = pd.DataFrame()

    for file in os.listdir(dir):
        if blacklist and file in blacklist:
            continue
        if whitelist and file not in whitelist:
            continue

        if file.endswith(ext):
            counter += 1
            if (not count_only) or (callback is not None):

                if verbose is True:
                    print('Loading file: ' + file)

                if file.endswith(".json"):
                    tmp_df = pd_read_json(dir + '/' + file)
                elif file.endswith((".csv", ".csv.gz")):
                    tmp_df = pd.read_csv(dir + '/' + file, low_memory=False)
                else:
                    print("Skipping unknown ext:" + file)
                    continue

                if verbose is True:
                    print('  -> Loaded items: ' + str(len(tmp_df)))

                if callback is not None:
                    callback(tmp_df)

                if not count_only:
                    dir_df = pd.concat([dir_df, tmp_df])

    print('Files found: ' + str(counter))

    if count_only:
        return counter
    else:
        return dir_df
