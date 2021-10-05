/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import React from 'react';
import clsx from 'clsx';
import Layout from '@theme/Layout';
import Link from '@docusaurus/Link';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import useBaseUrl from '@docusaurus/useBaseUrl';
import styles from './styles.module.css';

const featuresIntro = [
  {
    title: '',
    imageUrl: 'img/GeoLiftLogo.svg',
    description: (
      <>
      </>
    ),
  },
];

const features = [
  {
    title: 'Focus on True Business Outcomes',
    imageUrl: 'img/GeoLift_Grow.svg',
    description: (
      <>
        GeoLift is Facebook's open-source solution to measure Lift at
        a geographical level. By leveraging the latest advances in Synthetic
        Control Methods, GeoLift empowers you to start making decisions grounded
        in incrementality and measure the true value of your marketing campaigns.
      </>
    ),
  },
  {
    title: 'Reproducible & Transparent',
    imageUrl: 'img/GeoLift_Transparent.svg',
    description: (
      <>
        GeoLift offers an end-to-end solution to geo-experimentation which spans
        data ingestion, power analyses, market selection, and inference in an easy-
        to-use R package. Moreover, the tool's open-source nature makes it transparent
        and fully reproducible!
      </>
    ),
  },
  {
    title: 'Privacy Conscious & Resilient',
    imageUrl: 'img/GeoLift_Resilient.svg',
    description: (
      <>
        By relying exlcusively on aggregated market-level data and not personal
        information, GeoLift is resilient to signal loss all while protecting the
        consumer's privacy.
      </>
    ),
  },
];

function FeatureIntro({imageUrl, title, description}) {
  const imgUrl = useBaseUrl(imageUrl);
  return (
    <div className={clsx('col col--15', styles.feature)}>
      {imgUrl && (
        <div className="text--center">
          <img className={styles.featureImage} src={imgUrl} alt={title} />
        </div>
      )}
      <h3>{title}</h3>
      <p>{description}</p>
    </div>
  );
}

function Feature({imageUrl, title, description}) {
  const imgUrl = useBaseUrl(imageUrl);
  return (
    <div className={clsx('col col--4', styles.feature)}>
      {imgUrl && (
        <div className="text--center">
          <img className={styles.featureImage} src={imgUrl} alt={title} />
        </div>
      )}
      <h3>{title}</h3>
      <p>{description}</p>
    </div>
  );
}

export default function Home() {
  const context = useDocusaurusContext();
  const {siteConfig = {}} = context;
  return (
    <Layout
      title={`Hello from ${siteConfig.title}`}
      description="Description will go into a meta tag in <head />">
      <header className={clsx('hero hero--primary', styles.heroBanner)}>
        <div className="container">
          <h1 className="hero__title">{siteConfig.title}</h1>
          <p className="hero__subtitle">{siteConfig.tagline}</p>
          <div className={styles.buttons}>
            <Link
              className={clsx(
                'button button--outline button--secondary button--lg',
                styles.getStarted,
              )}
              to={useBaseUrl('docs/intro')}>
              Get Started
            </Link>
          </div>
        </div>
      </header>
      <main>
        {features && features.length > 0 && (
          <section className={styles.features}>
            <div className="container">
              <div className="row">
                {features.map(({title, imageUrl, description}) => (
                  <Feature
                    key={title}
                    title={title}
                    imageUrl={imageUrl}
                    description={description}
                  />
                ))}
              </div>
            </div>
          </section>
        )}
         {featuresIntro && featuresIntro.length > 0 && (
          <section className={styles.features}>
            <div className="container">
              <div className="row">
                {featuresIntro.map(({title, imageUrl, description}) => (
                  <FeatureIntro
                    key={title}
                    title={title}
                    imageUrl={imageUrl}
                    description={description}
                  />
                ))}
              </div>
            </div>
          </section>
        )}
      </main>
    </Layout>
  );
}
