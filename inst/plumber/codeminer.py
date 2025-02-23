import requests
import pandas as pd
from typing import List

BASE_URL = "http://0.0.0.0:8000"

def DESCRIPTION(reg_expr: str, code_type: str) -> pd.DataFrame:
    """
    Search for codes that match a description.

    Returns a DataFrame with clinical codes that match the supplied regular expression.

    Parameters:
    reg_expr (str): The regex pattern to search for.
    code_type (str): Type of clinical code system to be searched. One of "sct" (SNOMED CT) or "icd10" (ICD-10).

    Returns:
    pd.DataFrame: DataFrame of matching clinical codes.

    Example:
    >>> DESCRIPTION("cyst", "icd10")
    """
    response = requests.get(f"{BASE_URL}/DESCRIPTION", params={"reg_expr": reg_expr, "code_type": code_type})
    return pd.DataFrame(response.json())

def CODES(codes: List[str], code_type: str) -> pd.DataFrame:
    """
    Look up descriptions for clinical codes.

    Returns a DataFrame including descriptions for the codes of interest.

    Parameters:
    codes (list of str): Vector of codes to lookup.
    code_type (str): Type of clinical code system to be searched. One of "sct" (SNOMED CT) or "icd10" (ICD-10).

    Returns:
    pd.DataFrame: DataFrame of clinical codes and their descriptions.

    Example:
    >>> CODES(["E10", "E11"], "icd10")
    """
    response = requests.get(f"{BASE_URL}/CODES", params={"codes": codes, "code_type": code_type})
    return pd.DataFrame(response.json())

def CHILDREN(codes: List[str], code_type: str) -> pd.DataFrame:
    """
    Get descendents for a set of codes.

    Retrieves children codes for a given set of codes (including the codes themselves).

    Parameters:
    codes (list of str): A vector of code strings to retrieve child codes for.
    code_type (str): Type of clinical code system to be searched. One of "sct" (SNOMED CT) or "icd10" (ICD-10).

    Returns:
    pd.DataFrame: DataFrame of child codes.

    Example:
    >>> CHILDREN(["E10", "E11"], "icd10")
    """
    response = requests.get(f"{BASE_URL}/CHILDREN", params={"codes": codes, "code_type": code_type})
    return pd.DataFrame(response.json())
