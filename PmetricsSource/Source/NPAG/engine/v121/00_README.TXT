



2017Sep20

$ cp npagranfix_b03.f npagranfix_b04.f

npagranfix_b03.f -- YYYPRED is calcualted incorrectly; PRBTnnnn
    is completely wrong, DENnnnn and OUTTnnnn have  error. DO 6000
    and DO 7000 loops contain the error. CALL IDCALCY(...) and
    CALL IDCALCYY(...) appear to be correct, but the return
    values are 'obviously' incorrect.
