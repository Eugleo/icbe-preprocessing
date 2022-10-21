# Preprocessing the ICBe Dataset

_This is my best attempt at connecting the [documentation](https://docs.google.com/document/d/1aJkweohbfIWtNpJw1CmXbeIiK6czbJ5iPyKwiYP1YlU/edit#heading=h.61nwccqse1xi) to the column names in `ICBe_V1.1_events_agreed.Rds`. See below for identified problems._

Each crisis is split into sentences. Each sentence can contain multiple events. Each of the events can have the following attributes (or see the much nicer [html mindmap](./event.html)). 


- [do_duration]
- [...bunch of other stuff, like crisno and date...]
- [event_type], **NOT mutually exclusive!**
    - action
        - [do_actor_a]
        - [do_actor_b]
        - *implicit:* only one actor? (vs an interaction)
            - action
                - *implicit:* armed?
                    - armed
                        - *implicit:* escalatory?
                            - escalatory
                                - [act_escalate]
                                    - *contains the event label*
                            - deescalatory
                                - [act_deescalate]
                                    - *contains the event label*
                    - unarmed
                        - *implicit:* escalatory?
                            - escalatory
                                - [act_uncooperative]
                                    - *contains the event label*
                            - deescalatory
                                - [act_cooperative]
                                    - *contains the event label*
            - interaction
                - [...bunch of other stuff, like domain and units]
                - *implicit:* armed?
                    - armed
                        - *implicit:* escalatory?
                            - escalatory
                                - [interact_escalate]
                                    - *contains the event label*
                            - deescalatory
                                - [interact_deescalate]
                                    - *contains the event label*
                    - unarmed
                        - *implicit:* escalatory?
                            - escalatory
                                - [interact_increasecoop]
                                    - *contains the event label*
                            - deescalatory
                                - [interact_decreasecoop]
                                    - *contains the event label*
    - speech
        - [say_actor_a]
        - [say_actor_b]
        - [sayintkind_react]
            - *contains optional info about what sentences the speech refers to*
        - [sayintkind]
            - *contains the event label*
        - *implicit:* conditional?
            - conditional event
                - [condition_do_actor_a]
                - [condition_do_actor_b]
                - [condition]
                    - *contains info whether the condition is phrased as a positive or a negative statement, e.g. "if x DOES happen, then ..."*
                - [consequence]
                    - *contains info whether the consequence is phrased as a positive or a negative statement, e.g. "if ..., then y DOES happen"*
                - *implicit:* only one actor? (vs an interaction)
                    - action
                        - *implicit:* armed?
                            - armed
                                - *implicit:* escalatory?
                                    - escalatory
                                        - [condition_act_escalate]
                                            - *contains the event label*
                                    - deescalatory
                                        - [condition_act_deescalate]
                                            - *contains the event label*
                            - unarmed
                                - *implicit:* escalatory?
                                    - escalatory
                                        - [condition_act_uncooperative]
                                            - *contains the event label*
                                    - deescalatory
                                        - [condition_act_cooperative]
                                            - *contains the event label*
                    - interaction
                        - [...bunch of other stuff, like domain and units]
                        - *implicit:* armed?
                            - armed
                                - *implicit:* escalatory?
                                    - escalatory
                                        - [condition_interact_escalate]
                                            - *contains the event label
                                    - deescalatory
                                        - [condition_interact_deescalate]
                                            - *contains the event label*
                            - unarmed
                                - *implicit:* escalatory?
                                    - escalatory
                                        - [condition_interact_increasecoop]
                                            - *contains the event label*
                                    - deescalatory
                                        - [condition_interact_decreasecoop]
                                            - *contains the event label*
            - normal event
    - thought
        - [think_actor_a]
        - [thinkkind]
            - *contains the event label*
        - [think_sentence_events]
            - *contains optional info about what sentences the thinking is about*

# Questions
- What is the difference between do_kind and do_interact_kind?
    - It is not an action/interaction split

# Caveats

It seems that the coding process wasn't precisely defined:

- The coders were allowed to label one event with multiple event types (ie. both action AND thought)
    - e.g. they were allowed to say "this sentence has one event: an actionthought" instead of the more sensible "this sentence has two events: once action, one thought"
- The coders could input values for labels such as do_actor and interact_increasecoop even though they didn't label the event as action
    - So, they often labeled the event as thought, but also added labels for interaction types, action actors, etc.

This means that the tree structure of the labels isn't really that well preserved.

Additionally, in the final (agreed upon) wide table one row doesn't necessarily correspond to one event in a sentence, or even to one expert's opinion. It is aggregated from multiple opinions, but it doesn't represent the (only) consensus, either. 
