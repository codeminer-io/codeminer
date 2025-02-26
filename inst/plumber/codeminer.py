"""
This is a Python wrapper for the codeminer API.
Search strategies may be composed using the infix operators, e.g.:
`DESCRIPTION("diab", "icd10") >>AND>> (CHILDREN("E10", "icd10") >>OR>> CHILDREN("E11", "icd10"))`
"""

import requests
import pandas as pd
from typing import List
import os

DEFAULT_BASE_URL = "http://0.0.0.0:8000"

class CodeMinerError(Exception):
    """Custom exception for codeminer errors."""
    pass

def _validate_code_type(code_type: str):
    """Helper function to validate the code_type argument."""
    if code_type not in ["sct", "icd10"]:
        raise ValueError("code_type must be one of 'sct' or 'icd10'")

def _api_call(endpoint: str, params: dict, base_url: str = None) -> pd.DataFrame:
    """Helper function to make API calls."""
    effective_base_url = base_url or os.environ.get("CODEMINER_BASE_URL", DEFAULT_BASE_URL)
    try:
        response = requests.get(f"{effective_base_url}/{endpoint}", params=params)
        response.raise_for_status()  # Raise HTTPError for bad responses (4xx or 5xx)
        if not response.text:
            return pd.DataFrame()
        try:
            data = response.json()
        except ValueError:
            raise CodeMinerError("Invalid JSON received from the API")
        return pd.DataFrame(data)
    except requests.exceptions.RequestException as e:
        raise CodeMinerError(f"Error communicating with the codeminer API: {e}")

def DESCRIPTION(reg_expr: str, code_type: str, base_url: str = None) -> pd.DataFrame:
    """
    Search for codes that match a description.

    Returns a DataFrame with clinical codes that match the supplied regular expression.

    Parameters:
    reg_expr (str): The regex pattern to search for.
    code_type (str): Type of clinical code system to be searched. One of "sct" (SNOMED CT) or "icd10" (ICD-10).
    base_url (str, optional): The base URL of the codeminer API. Defaults to the CODEMINER_BASE_URL environment variable, or http://0.0.0.0:8000.

    Returns:
    pd.DataFrame: DataFrame of matching clinical codes.

    Example:
    >>> DESCRIPTION("cyst", "icd10")
    """
    _validate_code_type(code_type)
    return _api_call("DESCRIPTION", {"reg_expr": reg_expr, "code_type": code_type}, base_url)

def CODES(codes: List[str], code_type: str, base_url: str = None) -> pd.DataFrame:
    """
    Look up descriptions for clinical codes.

    Returns a DataFrame including descriptions for the codes of interest.

    Parameters:
    codes (list of str): Vector of codes to lookup.
    code_type (str): Type of clinical code system to be searched. One of "sct" (SNOMED CT) or "icd10" (ICD-10).
    base_url (str, optional): The base URL of the codeminer API. Defaults to the CODEMINER_BASE_URL environment variable, or http://0.0.0.0:8000.

    Returns:
    pd.DataFrame: DataFrame of clinical codes and their descriptions.

    Example:
    >>> CODES(["E10", "E11"], "icd10")
    """
    _validate_code_type(code_type)
    return _api_call("CODES", {"codes": codes, "code_type": code_type}, base_url)

def CHILDREN(codes: List[str], code_type: str, base_url: str = None) -> pd.DataFrame:
    """
    Get descendents for a set of codes.

    Retrieves children codes for a given set of codes (including the codes themselves).

    Parameters:
    codes (list of str): A vector of code strings to retrieve child codes for.
    code_type (str): Type of clinical code system to be searched. One of "sct" (SNOMED CT) or "icd10" (ICD-10).
    base_url (str, optional): The base URL of the codeminer API. Defaults to the CODEMINER_BASE_URL environment variable, or http://0.0.0.0:8000.

    Returns:
    pd.DataFrame: DataFrame of child codes.

    Example:
    >>> CHILDREN(["E10", "E11"], "icd10")
    """
    _validate_code_type(code_type)
    return _api_call("CHILDREN", {"codes": codes, "code_type": code_type}, base_url)

class Infix:
    def __init__(self, function):
        self.function = function

    def __rlshift__(self, other):
        return Infix(lambda x: self.function(other, x))

    def __lshift__(self, other):
        return self.function(other)

    def __rrshift__(self, other):
        return Infix(lambda x: self.function(other, x))

    def __rshift__(self, other):
        return self.function(other)

@Infix
def OR(df1: pd.DataFrame, df2: pd.DataFrame) -> pd.DataFrame:
    """
    Union of two DataFrames.

    Parameters:
    df1 (pd.DataFrame): First DataFrame.
    df2 (pd.DataFrame): Second DataFrame.

    Returns:
    pd.DataFrame: Union of the two DataFrames.

    Example:
    >>> df1 >>OR>> df2
    """
    return pd.concat([df1, df2]).drop_duplicates().reset_index(drop=True)

@Infix
def AND(df1: pd.DataFrame, df2: pd.DataFrame) -> pd.DataFrame:
    """
    Intersection of two DataFrames.

    Parameters:
    df1 (pd.DataFrame): First DataFrame.
    df2 (pd.DataFrame): Second DataFrame.

    Returns:
    pd.DataFrame: Intersection of the two DataFrames.

    Example:
    >>> df1 >>AND>> df2
    """
    return df1.merge(df2, how='inner')

@Infix
def NOT(df1: pd.DataFrame, df2: pd.DataFrame) -> pd.DataFrame:
    """
    Set difference of two DataFrames (df1 - df2).

    Parameters:
    df1 (pd.DataFrame): First DataFrame.
    df2 (pd.DataFrame): Second DataFrame.

    Returns:
    pd.DataFrame: Set difference of the two DataFrames.

    Example:
    >>> df1 >>NOT>> df2
    """
    return df1.merge(df2, how='left', indicator=True).query('_merge == "left_only"').drop(columns=['_merge'])
