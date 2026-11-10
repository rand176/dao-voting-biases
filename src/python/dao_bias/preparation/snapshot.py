"""Snapshot preparation extracted into explicit dataframe transformations."""
import json
import math
import re
import numpy as np
import pandas as pd

def prepare_language(proposals, *, detect=None):
    df_proposals = proposals.copy()
    if detect is None:
        from langdetect import detect
    def check_chinese(txt, threshold):
        if type(txt) == float or txt == "":
            return 0
        counter = 0
        T = math.floor(len(txt) * threshold)
        for c in txt:
            res = re.findall('[\\u4e00-\\u9fff]+', c)
            if res:
                counter += 1
                if counter > T:
                    return 1
        return 1 if len(res) > 0 else 0
    df_proposals['title_is_chinese'] = df_proposals['title'].apply(check_chinese, threshold=0.1)

    def check_korean(txt, threshold):
        if type(txt) == float or txt == "":
            return 0
        counter = 0
        T = math.floor(len(txt) * threshold)
        for c in txt:
            res = re.findall('[\\u3131-\\ucb4c]+', c)
            if res:
                counter += 1
                if counter > T:
                    return 1
        return 0
    df_proposals['title_is_korean'] = df_proposals['title'].apply(check_korean, threshold=0.1)

    def revise_korean(row):
        return 1 if (row['title_is_korean'] == 1) & (row['title_is_chinese'] == 0) else 0
    df_proposals['title_is_korean'] = df_proposals.apply(revise_korean, axis=1)
    ranges = [{'from': ord(u'㌀'), 'to': ord(u'㏿')}, {'from': ord(u'︰'), 'to': ord(u'﹏')}, {'from': ord(u'豈'), 'to': ord(u'\ufaff')}, {'from': ord(u'丽'), 'to': ord(u'\U0002fa1f')}, {'from': ord(u'\u3040'), 'to': ord(u'ゟ')}, {'from': ord(u'゠'), 'to': ord(u'ヿ')}, {'from': ord(u'⺀'), 'to': ord(u'\u2eff')}, {'from': ord(u'一'), 'to': ord(u'鿿')}, {'from': ord(u'㐀'), 'to': ord(u'䶿')}, {'from': ord(u'𠀀'), 'to': ord(u'𪛟')}, {'from': ord(u'𪜀'), 'to': ord(u'\U0002b73f')}, {'from': ord(u'𫝀'), 'to': ord(u'\U0002b81f')}, {'from': ord(u'𫠠'), 'to': ord(u'\U0002ceaf')}]

    def is_cjk(char):
        return any([range['from'] <= ord(char) <= range['to'] for range in ranges])

    def check_japanese(txt, threshold):
        if type(txt) == float or txt == "":
            return 0
        counter = 0
        T = math.floor(len(txt) * threshold)
        for c in txt:
            res = is_cjk(c)
            if res:
                counter += 1
                if counter > T:
                    return 1
        return 0
    df_proposals['title_is_japanese'] = df_proposals['title'].apply(check_japanese, threshold=0.1)

    def revise_japanese(row):
        return 1 if (row['title_is_japanese'] == 1) & (row['title_is_chinese'] == 0) else 0
    df_proposals['title_is_japanese'] = df_proposals.apply(revise_japanese, axis=1)
    df_proposals['body_is_chinese_all'] = df_proposals['body'].apply(check_chinese, threshold=0)
    df_proposals['body_is_chinese'] = df_proposals['body'].apply(check_chinese, threshold=0.3)

    def revise_korean_body(row):
        return 1 if (row['body_is_korean'] == 1) & (row['body_is_chinese'] == 0) else 0
    df_proposals['body_is_korean'] = df_proposals['body'].apply(check_korean, threshold=0.3)
    df_proposals['body_is_korean'] = df_proposals.apply(revise_korean_body, axis=1)

    def revise_japanese_body(row):
        return 1 if (row['body_is_japanese'] == 1) & (row['body_is_chinese'] == 0) else 0
    df_proposals['body_is_japanese'] = df_proposals['body'].apply(check_japanese, threshold=0.3)
    df_proposals['body_is_japanese'] = df_proposals.apply(revise_japanese_body, axis=1)

    def detect_lang_of_txt(title):
        res = 'unknown'
        try:
            res = detect(str(title))
        except:
            pass
        return res
    df_proposals['title_lang'] = df_proposals['title'].apply(detect_lang_of_txt)
    df_proposals['body_lang'] = df_proposals['body'].apply(detect_lang_of_txt)
    return df_proposals

def prepare_lengths(proposals):
    df_proposals = proposals.copy()
    Tmin = 10
    Tmin2 = 5

    def title_len(df):
        df['title_mod'] = df['title'].str.strip()
        df['title_mod'] = df['title_mod'].str.lower()
        df['title_mod_len'] = df['title_mod'].apply(lambda x: 0 if isinstance(x, float) else len(x))
        df['title_is_short'] = df['title_mod_len'].apply(lambda x: 1 if x < Tmin else 0)
        df['title_is_very_short'] = df['title_mod_len'].apply(lambda x: 1 if x < Tmin2 else 0)
        return df
    df_proposals = title_len(df_proposals)

    def _body_len(body):
        if type(body) == float:
            return 0
        return len(body)
    Tmin = 50
    Tmin2 = 10

    def body_len(df):
        df['body_mod'] = df['body'].str.strip()
        df['body_mod'] = df['body_mod'].str.lower()
        df['body_mod_len'] = df['body_mod'].apply(_body_len)
        df['body_is_short'] = df['body_mod_len'].apply(lambda x: 1 if x < Tmin else 0)
        df['body_is_very_short'] = df['body_mod_len'].apply(lambda x: 1 if x < Tmin2 else 0)
        return df
    df_proposals = body_len(df_proposals)
    return df_proposals

def prepare_maturity(proposals, spaces, follows):
    df_spaces = spaces.copy()
    df_follows = follows.copy()
    df_proposals_clean = proposals.copy()
    df_proposals_all = df_proposals_clean
    spaces_1prop = df_proposals_clean.groupby('space').count()['id'].reset_index()
    spaces_1prop = spaces_1prop.loc[spaces_1prop['id'] == 1]
    df_proposals_clean = df_proposals_clean.loc[~df_proposals_clean['space'].isin(spaces_1prop['space'])]
    spaces_at_most_2voters = df_proposals_clean.groupby('space')['votes'].max().reset_index()
    spaces_at_most_2voters = spaces_at_most_2voters.loc[spaces_at_most_2voters['votes'] < 3]
    df_proposals_clean = df_proposals_clean.loc[~df_proposals_clean['space'].isin(spaces_at_most_2voters['space'])]
    spaces_less_5follows = df_follows.value_counts('space').reset_index()
    spaces_less_5follows.columns = ['space', 'count']
    spaces_less_5follows = spaces_less_5follows.loc[spaces_less_5follows['count'] < 5]
    df_proposals_clean = df_proposals_clean.loc[~df_proposals_clean['space'].isin(spaces_less_5follows['space'])]
    df_spaces_clean = df_spaces.loc[df_spaces['id'].isin(df_proposals_clean['space'])]
    df_proposals_all['paper'] = df_proposals_all['id'].isin(df_proposals_clean['id']).astype(float)
    return df_proposals_all, df_spaces_clean

def prepare_choices(proposals):
    df_proposals = proposals.copy()
    def try_load_choices(choices):
        try:
            return json.loads(choices)
        except:
            return []
    df_proposals['choices_orig'] = df_proposals['choices']
    df_proposals['choices'] = df_proposals['choices'].apply(try_load_choices)
    df_proposals['scores_orig'] = df_proposals['scores']
    df_proposals['scores'] = df_proposals['scores'].apply(try_load_choices)
    df_proposals = df_proposals[df_proposals['choices'].apply(lambda x: isinstance(x, list))]

    def check_len(row, col):
        x = row[col]
        return len(x)
    df_proposals['len_choices'] = df_proposals.apply(check_len, axis=1, args=('choices',))
    df_proposals['len_scores'] = df_proposals.apply(check_len, axis=1, args=('scores',))
    df_proposals['diff_len_scores'] = df_proposals['len_scores'] - df_proposals['len_choices']
    bb = df_proposals.loc[df_proposals['scores_state'] == 'final']
    df_proposals = bb
    count_err = 0

    def get_win_choice(prop):
        nonlocal count_err
        scores = prop['scores']
        if type(scores) == float or len(scores) == 0:
            count_err += 1
            return [-1]
        largest = max(scores)
        win_choices = [g for g, x in enumerate(scores) if x == largest]
        return win_choices

    def pos_stats(df):
        df['win_choices'] = df.apply(get_win_choice, axis=1)
        df['win_choices_len'] = df['win_choices'].apply(lambda x: 0 if x == -1 else len(x))
        df['first_choice_won'] = df['win_choices'].apply(lambda x: 1 if 1 in x else 0)
        return df
    df_proposals = pos_stats(df_proposals)
    df_proposals = df_proposals.loc[df_proposals['space'] != 'polarcat.eth']
    df_proposals.drop(columns=['len_scores', 'len_choices', 'scores_orig', 'choices_orig', 'diff_len_scores'], inplace=True)
    df_choices = df_proposals.explode('choices').reset_index()
    df_choices.rename(columns={'index': 'prop_idx'}, inplace=True)

    def adjust_df_choices(df):
        old_prop_idx = '-100000'
        df['choice_idx'] = 0
        df['choice_text'] = pd.Series(0, index=df.index, dtype=object)
        df['choice_score'] = 0.0
        df['choice_won'] = 0
        for idx, row in df.iterrows():
            prop_idx = row['prop_idx']
            if prop_idx != old_prop_idx:
                old_prop_idx = prop_idx
                choice_counter = 0
            else:
                choice_counter += 1
            choice = row['choices']
            df.loc[idx, 'choice_idx'] = choice_counter + 1
            df.loc[idx, 'choice_text'] = choice
            if type(choice) == float:
                df.loc[idx, 'choice_text_len'] = 0
            elif type(choice) == int:
                df.loc[idx, 'choice_text_len'] = len(str(choice))
            else:
                df.loc[idx, 'choice_text_len'] = len(choice)
            if type(row['scores']) == float or len(row['scores']) == 0:
                df.loc[idx, 'choice_score'] = 0
                df.loc[idx, 'choice_won'] = 0
            else:
                df.loc[idx, 'choice_score'] = row['scores'][choice_counter]
                df.loc[idx, 'choice_won'] = 1 if choice_counter in row['win_choices'] else 0
        return df
    df_choices = adjust_df_choices(df_choices)
    df_choices.rename(columns={'id': 'proposal'}, inplace=True)
    df_choices.drop(columns=['prop_idx'], inplace=True)
    return df_choices

def prepare_delegates(proposals, spaces):
    df_proposals = proposals.copy()
    df_spaces = spaces.copy()
    def find_deleg_strategies(row):
        strategies = row['strategies']
        res = []
        if type(strategies) != float and 'delegat' in strategies:
            strats = json.loads(strategies)
            for j, strat in enumerate(strats):
                if 'name' in strat:
                    name = strat['name']
                    if 'delegat' in name:
                        res.append(name)
        return res
    df_proposals['deleg_strategies'] = df_proposals.apply(find_deleg_strategies, axis=1)
    df_proposals['n_deleg_strategies'] = df_proposals['deleg_strategies'].apply(len)
    df_proposals['has_deleg_strategies'] = df_proposals['n_deleg_strategies'].apply(lambda x: 1 if x > 0 else 0)
    cols = ['id', 'deleg_strategies', 'n_deleg_strategies', 'has_deleg_strategies']
    voting_keys = set()
    filters_keys = set()
    plugins_keys = set()
    do_print = False

    def parse_settings(row):
        try:
            s = json.loads(row['settings'])
            type = 'NA'
            delay = 'NA'
            period = 'NA'
            hideAbstain = 'NA'
            quorum = 'NA'
            privacy = 'NA'
            if 'voting' in s:
                v = s['voting']
                if 'type' in v:
                    type = v['type']
                if 'hideAbstain' in v:
                    hideAbstain = 1 if v['hideAbstain'] == True else 0
                if 'delay' in v:
                    delay = v['delay']
                if 'period' in v:
                    period = v['period']
                if 'quorum' in v:
                    quorum = v['quorum']
                if 'privacy' in v:
                    privacy = v['privacy']
                k = v.keys()
                voting_keys.update(k)
                if do_print:
                    print('voting ' + str(v))
            minScore = 'NA'
            onlyMembers = 'NA'
            defaultTab = 'NA'
            invalids = 'NA'
            if 'filters' in s:
                f = s['filters']
                if 'minScore' in f:
                    minScore = f['minScore']
                if 'invalids' in f:
                    invalids = f['invalids']
                if 'defaultTab' in f:
                    defaultTab = f['defaultTab']
                if 'onlyMembers' in f:
                    onlyMembers = 1 if f['onlyMembers'] == True else 0
                k = f.keys()
                filters_keys.update(k)
                if do_print:
                    print('filters ' + str(f))
            poap = 'NA'
            aragon = 'NA'
            safeSnap = 'NA'
            subscribe = 'NA'
            commentBox = 'NA'
            aragon = 'NA'
            charts = 'NA'
            commentBox = 'NA'
            commentBox2 = 'NA'
            daoModule = 'NA'
            gnosis = 'NA'
            hal = 'NA'
            myVote = 'NA'
            oSnap = 'NA'
            poap = 'NA'
            progress = 'NA'
            projectGalaxy = 'NA'
            quorum = 'NA'
            safeSnap = 'NA'
            safesnap = 'NA'
            subscribe = 'NA'
            wiz = 'NA'
            if 'plugins' in s:
                p = s['plugins']
                poap = 1 if 'poap' in p else 0
                aragon = 1 if 'aragon' in p else 0
                safeSnap = 1 if 'safeSnap' in p else 0
                subscribe = 1 if 'subscribe' in p else 0
                commentBox = 1 if 'commentBox' in p else 0
                charts = 1 if 'charts' in p else 0
                commentBox = 1 if 'commentBox' in p else 0
                commentBox2 = 1 if 'commentBox2' in p else 0
                daoModule = 1 if 'daoModule' in p else 0
                gnosis = 1 if 'gnosis' in p else 0
                hal = 1 if 'hal' in p else 0
                myVote = 1 if 'myVote' in p else 0
                oSnap = 1 if 'oSnap' in p else 0
                progress = 1 if 'progress' in p else 0
                projectGalaxy = 1 if 'projectGalaxy' in p else 0
                quorum = 1 if 'quorum' in p else 0
                safesnap = 1 if 'commentBox' in p else 0
                wiz = 1 if 'commentBox' in p else 0
                k = p.keys()
                plugins_keys.update(k)
                if do_print:
                    print('plugins ' + str(p))
            return [row['id'], type, hideAbstain, delay, period, quorum, privacy, minScore, onlyMembers, invalids, defaultTab, aragon, charts, commentBox, commentBox2, daoModule, gnosis, hal, myVote, oSnap, poap, progress, projectGalaxy, quorum, safeSnap, safesnap, subscribe, wiz]
        except:
            print('err ' + s)
    mydata = df_spaces
    res = mydata.apply(parse_settings, axis=1).to_list()
    cols = ['id', 'vo_type', 'vo_hideAbstain', 'vo_delay', 'vo_period', 'vo_quorum', 'vo_privacy', 'fl_minScore', 'fl_onlyMembers', 'fl_invalids', 'fl_defaultTab', 'pl_aragon', 'pl_charts', 'pl_commentBox', 'pl_commentBox2', 'pl_daoModule', 'pl_gnosis', 'pl_hal', 'pl_myVote', 'pl_oSnap', 'pl_poap', 'pl_progress', 'pl_projectGalaxy', 'pl_quorum', 'pl_safeSnap', 'pl_safesnap', 'pl_subscribe', 'pl_wiz']
    df_spaces_settings = pd.DataFrame(res, columns=cols)
    return (df_proposals[["id", "deleg_strategies", "n_deleg_strategies", "has_deleg_strategies"]], df_spaces_settings)

def prepare_dates(proposals, spaces):
    df_proposals = proposals.copy()
    df_spaces = spaces.copy()
    df_proposals['YMD_start'] = pd.to_datetime(df_proposals['start'], unit='s').dt.strftime('%Y-%m-%d')
    df_proposals['YM_start'] = pd.to_datetime(df_proposals['start'], unit='s').dt.strftime('%Y-%m')
    df_proposals['Y_start'] = pd.to_datetime(df_proposals['start'], unit='s').dt.strftime('%Y')
    df_proposals['YMD_end'] = pd.to_datetime(df_proposals['end'], unit='s').dt.strftime('%Y-%m-%d')
    df_proposals['YM_end'] = pd.to_datetime(df_proposals['end'], unit='s').dt.strftime('%Y-%m')
    df_proposals['Y_end'] = pd.to_datetime(df_proposals['end'], unit='s').dt.strftime('%Y')
    df_proposals['YMD_created'] = pd.to_datetime(df_proposals['created'], unit='s').dt.strftime('%Y-%m-%d')
    df_proposals['YM_created'] = pd.to_datetime(df_proposals['created'], unit='s').dt.strftime('%Y-%m')
    df_proposals['Y_created'] = pd.to_datetime(df_proposals['created'], unit='s').dt.strftime('%Y')
    df_spaces = df_spaces[['id', 'created']]
    df_spaces.columns = ['space', 'space_created']
    df_proposals = pd.merge(df_proposals, df_spaces, on='space', how='left')
    df_proposals['created_relative'] = df_proposals.apply(lambda x: x['created'] - x['space_created'], axis=1)
    df_prop_before_space = df_proposals.loc[df_proposals['created_relative'] < 0]
    df_prop = df_proposals.set_index('id')
    group = df_prop.groupby(['space'])
    group = group.cumcount() + 1
    df_prop_counter = group.reset_index(name='prop_counter')
    df_proposals_clean = df_proposals.loc[~df_proposals['id'].isna()]
    df_proposals_id_spaces = df_proposals_clean[df_proposals_clean['id'].str.contains('\\s+')]
    df_proposals_old = df_proposals
    df_proposals2 = pd.merge(df_proposals, df_prop_counter, on='id', how='left')
    ddf = df_proposals.loc[df_proposals.id.duplicated()]
    for IDX in range(1, len(ddf)):
        print(ddf.iloc[IDX])
    df_prop_counter = df_prop_counter.loc[~df_prop_counter.id.isna()]
    dupi = df_proposals.loc[df_proposals.id.duplicated()].id
    df_prop_counter = df_prop_counter.loc[~df_prop_counter.id.isin(dupi)]
    df_proposals2 = pd.merge(df_proposals, df_prop_counter, on='id', how='left')
    cols = ['id', 'YMD_start', 'YM_start', 'Y_start', 'YMD_end', 'YM_end', 'Y_end', 'YMD_created', 'YM_created', 'Y_created', 'space_created', 'created_relative', 'prop_counter']
    return df_proposals2[cols]
