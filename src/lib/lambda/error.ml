open Common.Ast

exception UntypeableLet of term
exception UntypeableFix of term
exception FVNotFound    of term

