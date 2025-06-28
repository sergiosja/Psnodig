# Psnodig

A general transpiler with options for pseudocode and flowcharts. It was developed in conjunction with my master's thesis. \

Psnodig has a parser for the following languages:
- Gourmet

Psnodig has a writer for the following formats:
- Gourmet
- Python
- Pseudocode
- Flowcharts

The flowchart writer is a prototype, as it still struggles with complex programs (particularly the presence of nested control flow statements).

# Use

First you must clone the repository. Then run

```
stack install
```

To execute a Gourmet program, run

```
psnodig <program.gt>
```

To transpile a Gourmet program to pseudocode (LaTeX and PDF), run

```
psnodig tbp pdf <program.gt>
```

To transpile a Gourmet program to a flowchart (LaTeX and PDF), run

```
psnodig ibp pdf <program.gt>
```

You can also drop `pdf` if you only want the LaTeX file. To transpile a Gourmet program to Gourmet and Python, you can run

```
psnodig gourmet <program.gt>
```

```
psnodig python <program.gt>
```


# Post submission additions

Here are all additions to the source code, after I submitted the thesis 15th of May.

- Removal of old files (commit: d39a597919ed7f55de9a91d69b870cc2e04dd25d).
- Updated unit tests of Python writer (commit: a0e8e219094659c09c60344b46036f04e98d5133).
