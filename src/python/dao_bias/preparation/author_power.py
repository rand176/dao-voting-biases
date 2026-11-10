"""No-author-voting-power sensitivity transform, preserving ballot support fractions."""
import numpy as np
import pandas as pd

def remove_author_power(choices):
    df = choices.copy()
    multi_choice_types = ~df['type'].isin(['basic', 'single-choice'])
    author_choice_counts = df[multi_choice_types & (df['is_author_choice'] == 1.0)].groupby('proposal')['is_author_choice'].sum().rename('author_n_choices')
    df = df.join(author_choice_counts, on='proposal')
    df['author_n_choices'] = df['author_n_choices'].fillna(1)
    author_vp_contribution = pd.Series(0.0, index=df.index)
    support_fraction_available = pd.Series(False, index=df.index)
    if 'author_support_fraction' in df.columns:
        author_support_fraction = pd.to_numeric(df['author_support_fraction'], errors='coerce')
        support_fraction_available = author_support_fraction.notna().groupby(df['proposal']).transform('any')
        author_support_fraction = author_support_fraction.mask(support_fraction_available & author_support_fraction.isna(), 0.0)
        supported_rows = support_fraction_available & author_support_fraction.notna() & (author_support_fraction > 0)
        author_vp_contribution.loc[supported_rows] = df.loc[supported_rows, 'author_vp'] * author_support_fraction.loc[supported_rows]
    is_author_choice = df['is_author_choice'].notna() & (df['is_author_choice'] == 1.0)
    fallback_rows = ~support_fraction_available & is_author_choice
    vp_share = df['author_vp'] / df['author_n_choices']
    author_vp_contribution.loc[fallback_rows] = vp_share.loc[fallback_rows]
    df['choice_score_no_author_vp'] = (df['choice_score'] - author_vp_contribution).clip(lower=0)
    df['choice_share_no_author_vp'] = df.groupby('proposal')['choice_score_no_author_vp'].transform(lambda x: x / x.sum() if x.sum() != 0 else np.nan)
    df['max_choice_share_no_author_vp'] = df.groupby('proposal')['choice_share_no_author_vp'].transform('max')
    is_max = np.isclose(df['choice_share_no_author_vp'], df['max_choice_share_no_author_vp'])
    df['first_choice_won_no_author_vp'] = ((df['choice_idx'] == 1) & is_max & (df['choice_score_no_author_vp'] > df['quorum'])).astype(int)
    df['choice_won_no_author_vp'] = (is_max & (df['choice_score_no_author_vp'] > df['quorum'])).astype(int)
    df.drop(columns=['max_choice_share_no_author_vp', 'author_n_choices'], inplace=True)
    return df
