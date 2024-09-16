// touch: 1
#import "@preview/imprecv:1.0.1": *

#let cvdata = yaml("cv.yml")

#let uservars = (
    headingfont: "Linux Libertine",
    bodyfont: "Linux Libertine",
    fontsize: 10pt, // 10pt, 11pt, 12pt
    linespacing: 6pt,
    sectionspacing: 0pt,
    showAddress:  false, // true/false show address in contact info
    showNumber: false,  // true/false show phone number in contact info
    showTitle: true,   // true/false show title in heading
    headingsmallcaps: false, // true/false use small caps for headings
    sendnote: false, // set to false to have sideways endnote
)

// setrules and showrules can be overridden by re-declaring it here
// #let setrules(doc) = {
//      // add custom document style rules here
//
//      doc
// }

#let customrules(doc) = {
    // add custom document style rules here
    set page(
        paper: "us-letter",     // a4, us-letter
        numbering: "1 / 1",     // you can comment out or remove this line to remove numbering
        number-align: center,   // left, center, right
        margin: 1.25cm,         // 1.25cm, 1.87cm, 2.5cm
    )

    // set list(indent: 1em)   // indent bullet list for legibility

    doc
}

#let cvinit(doc) = {
    doc = setrules(uservars, doc)
    doc = showrules(uservars, doc)
    doc = customrules(doc)

    doc
}

// each section body can be overridden by re-declaring it here
// #let cveducation = []

// ========================================================================== //

#show: doc => cvinit(doc)

#cvheading(cvdata, uservars)
#cvwork(cvdata)
#cveducation(cvdata)
#cvprojects(cvdata)
#cvaffiliations(cvdata)
// #cvawards(cvdata)
// #cvcertificates(cvdata)
// #cvpublications(cvdata)
#cvskills(cvdata)
// #cvreferences(cvdata)
#endnote(uservars)
