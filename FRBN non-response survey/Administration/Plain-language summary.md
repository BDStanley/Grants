# What this project does, in plain language

**Project:** Pilot implementation of a randomized response instrument in a Polish socio-political survey
**Team:** Ben Stanley (PI), Mikołaj Cześnik, Marta Żerkowska-Balas — SWPS University, Centre for the Study of Democracy
**Funding:** SWPS FRBN internal grant, decision 67/2026/FRBN/C

---

## The problem: the people who answer surveys are not the people who don't

Almost everything we know about public opinion — who's ahead in the polls, how many people will vote, what Poles think about democracy or minorities — comes from surveys. And surveys have a dirty secret: **almost nobody answers them anymore**. In a typical online panel survey, for every hundred people invited, perhaps ten take part.

That wouldn't matter if the ten who answer were just like the ninety who don't. Pollsters try to patch the difference with **weighting**: if the sample has too few young people or too few rural residents, their answers are counted more heavily until the sample "looks like Poland" on age, gender, education and where people live.

But weighting has a blind spot. It can only fix imbalances on things we can *see and measure* — demographics. It silently assumes that once the demographics match, a respondent is interchangeable with the non-respondents who share their profile: that a 45-year-old woman with secondary education who *takes* the survey answers the same way as the 45-year-old women with secondary education who *ignored the invitation*.

For many questions that assumption is fine. For some — and often exactly the ones political scientists care most about — it is not. People who love talking about politics are much more likely to take a political survey. People who distrust institutions, feel their views are frowned upon, or simply don't care about politics quietly disappear from the data. When *the very thing you're trying to measure* affects whether someone answers, statisticians call it **non-ignorable nonresponse** — and no amount of demographic weighting can fix it, because the missing people differ in their *opinions*, not just their age or education.

This is not a hypothetical worry. It is the best current explanation of why polls in several countries have repeatedly misjudged election results, and it plausibly distorts Polish surveys too — declared election turnout in Polish polls, for example, routinely comes out far above real turnout.

## Bailey's insight: the bias leaves fingerprints — if you design the survey to reveal them

The project builds directly on Michael Bailey's book *Polling at a Crossroads* (Cambridge University Press, 2024), which argues that survey research has reached a fork in the road: keep weighting and hoping, or start *testing* whether the hoping is justified. His reasoning runs in three steps.

**Step 1: Think of respondents as queuing by eagerness.** Imagine everyone invited to a survey standing in a line, ordered by how keen they are to take part. A survey with a low response rate only ever hears from the front of the queue — the most eager. A survey that pushes harder (reminders, encouragement) reaches deeper into the queue and picks up more of the reluctant people too.

**Step 2: Compare the front of the queue with the middle.** Here is the trick. Suppose you run *the same survey two ways at once*, randomly deciding which invitees get the "gentle" version (one invitation, then silence) and which get the "insistent" version (invitation plus reminders). The gentle version captures only the eager front of the queue. The insistent version captures the eager *and* a slice of the reluctant.

Now compare their answers, question by question:

- If eager and reluctant respondents answer a question **the same way**, the two versions give the same result. That's evidence the question is *safe*: the people you're missing probably resemble the people you've got, and ordinary weighting is trustworthy for that question.
- If the two versions give **different results**, you've caught the bias red-handed. Willingness to respond is linked to the answer itself — and if the *somewhat* reluctant differ from the eager, the people who never answered at all (who are even more reluctant) likely differ even more. The published survey figure is skewed, and weighting won't rescue it.

Because who-gets-which-version is decided by a coin flip, any difference between the versions can't be blamed on the two groups being different kinds of people to start with. The random assignment is what turns a hunch into a measurement — it's what the technical term **"randomized response instrument"** means: a randomly assigned nudge that changes *whether* people respond without changing *what they think*.

**Step 3: Use the fingerprint to correct the estimate.** Once you can see *how much* answers shift as you move from eager to reluctant respondents, statistical models (so-called selection models, descended from work that won James Heckman a Nobel Prize) can extrapolate that trend to the people who never answered, producing corrected estimates — plus honest uncertainty about them. Bailey's key point is that these models were long dismissed as unreliable, but they fail mainly when fed the wrong *data*. Give them a well-designed experiment like the one above and they work. In his words, borrowed from statistician Donald Rubin: **design trumps analysis**.

When Bailey ran this design in the United States, the results were striking. Declared turnout was heavily inflated by eager respondents. Overall presidential approval looked unbiased — but only because two biases cancelled: the Democrats who responded were more anti-Trump than typical Democrats, and the Republicans who responded were more pro-Trump than typical Republicans. Polls were quietly *exaggerating how polarized the country is*.

## What we will actually do

Nobody has systematically tested this approach outside the Anglosphere. We will run the first such test in Poland:

1. **One survey, two contact protocols.** A Polish online panel agency will invite a large random sample to a ~10-minute survey on politics and society. Half the invitees (chosen at random) get the standard treatment: an invitation plus two reminders. The other half get a single invitation and no reminders. Roughly 2,000 people will complete the survey. Every completed interview is tagged with its protocol.
2. **Questions chosen to put the method to work.** The questionnaire mixes items where we *expect* the bias to appear — will you vote? which party do you support (including parties carrying a social stigma)? attitudes towards minorities; commitment to democratic norms — with control questions where we expect no bias (e.g., basic policy views, and a few behaviours like smoking, where official statistics tell us the true population answer).
3. **Everything pre-registered.** Before any data are collected, we publicly deposit our hypotheses, tests and decision rules, so we can't fool ourselves (or be accused of it) afterwards.
4. **Diagnose, then correct.** For each question we test whether the two protocols yield different answers. Where they don't, we can certify that standard weighting is adequate — a genuinely useful, reassuring finding. Where they do, we apply selection models to produce corrected estimates, and check them against hard benchmarks such as official turnout and election results.
5. **Share everything.** All analysis code, anonymized data and a step-by-step implementation guide will be published openly, and the findings presented in a workshop to the team behind the Polish National Election Study (PGSW).

## Why it matters

- **For Polish survey research:** this delivers a cheap, reusable diagnostic — essentially, randomly varying how hard a survey pushes for responses, something agencies do anyway — that any Polish study can bolt on to find out *which* of its questions are distorted by nonresponse and by how much. The immediate beneficiary is PGSW, the flagship academic study of Polish elections.
- **For public debate:** polls shape coverage, expectations and even policy. Knowing which polled quantities are trustworthy (and which are inflated — turnout being the prime suspect) is a public good, and the project includes an accessible report for journalists who work with polls.
- **For science:** Poland — with its polarized politics and stigmatized parties — is an ideal stress test for a method developed in the US. The results feed a full-scale follow-up (NCN OPUS, Horizon Europe) and an article in a leading survey-methodology journal.

The one-sentence version: **instead of assuming the people who ignore our surveys think like the people who answer them, we run a small randomized experiment inside the survey that reveals — question by question — whether that assumption is true, and repairs the numbers where it isn't.**
